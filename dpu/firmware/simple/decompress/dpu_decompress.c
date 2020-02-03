#include <attributes.h>
#include <defs.h>
#include <mram.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "dpu_decompress.h"

#define ZSTD_MAGIC_NUMBER (0xFD2FB528U)
#define MAX_LITERALS_SIZE ((size_t)128 * 1024)
#define HUF_MAX_SYMBOLS (256)
#define HUF_MAX_BITS (16)
#define HUF_MAX_SIZE (1 << HUF_MAX_BITS)

#define FSE_MAX_ACCURACY_LOG (15)
#define FSE_USED_ACCURACY_LOG (7)
#define FSE_MAX_SYMBS (256)
#define FSE_MAX_SIZE (1 << ((FSE_USED_ACCURACY_LOG > 9) ? FSE_USED_ACCURACY_LOG : 9))

#define FSE_NR_TABLES (4)
#define FSE_LITERALS_TABLE_IDX (0)
#define FSE_LL_TABLE_IDX (1)
#define FSE_OF_TABLE_IDX (2)
#define FSE_ML_TABLE_IDX (3)

#define MRAM_CACHE_SIZE 8

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

#define unlikely(x) __builtin_expect(x, 0)

typedef enum {
    RAW_BLOCK = 0,
    RLE_BLOCK = 1,
    COMPRESSED_BLOCK = 2,
    RESERVED_BLOCK = 3,
} block_type_t;

typedef enum {
    OK = 0,
    INVALID_MAGIC = 1,
    OUT_TOO_SMALL = 2,
    IN_TOO_SMALL = 3,
    IN_NOT_ALIGNED = 4,
    CORRUPTION = 5,
    IMPOSSIBLE = 6,
    WRONG_DICTIONARY = 7,
    HUF_TOO_MANY_SYMBS = 8,
    HUF_TABLE_DEPTH_TOO_LARGE = 9,
    FSE_ACCURACY_TOO_LARGE = 10,
} diagnostic_t;

__host diagnostic_t dpu_status[NR_TASKLETS] = {};

#define ERROR(n)                                                                                                                 \
    do {                                                                                                                         \
        dpu_status[me()] = (n);                                                                                                  \
        halt();                                                                                                                  \
    } while (0)

static __mram_noinit u8 literals_storage[NR_TASKLETS][MAX_LITERALS_SIZE];
static __mram_noinit u8 huf_symbols[NR_TASKLETS][HUF_MAX_SIZE];
static __mram_noinit u8 huf_num_bits[NR_TASKLETS][HUF_MAX_SIZE];
static __mram_noinit u8 fse_symbols[NR_TASKLETS][FSE_NR_TABLES][FSE_MAX_SIZE];
static __mram_noinit u8 fse_num_bits[NR_TASKLETS][FSE_NR_TABLES][FSE_MAX_SIZE];
static __mram_noinit u16 fse_new_state_base[NR_TASKLETS][FSE_NR_TABLES][FSE_MAX_SIZE];

static __dma_aligned u8 in_cache[NR_TASKLETS][MRAM_CACHE_SIZE];
static __dma_aligned u8 out_cache[NR_TASKLETS][MRAM_CACHE_SIZE];

typedef struct {
    mram(u8 *) ptr;
    size_t len;
} mram_ostream_t;

typedef struct {
    const mram(u8 *) ptr;
    size_t len;
    i8 bit_offset;
} mram_istream_t;

typedef struct {
    mram(u8 *) symbols;
    mram(u8 *) num_bits;
    i32 max_bits;
} HUF_dtable_t;

typedef struct {
    mram(u8 *) symbols;
    mram(u8 *) num_bits;
    mram(u16 *) new_state_base;
    i32 accuracy_log;
} FSE_dtable_t;

typedef struct {
    HUF_dtable_t literals_dtable;
    FSE_dtable_t ll_dtable;
    FSE_dtable_t ml_dtable;
    FSE_dtable_t of_dtable;

    mram(u8 *) content;
    size_t content_size;

    u32 previous_offsets[3];
    u32 dictionary_id;
} dictionary_t;

typedef struct {
    size_t window_size;
    size_t frame_content_size;
    u32 dictionary_id;
    bool content_checksum_flag;
    bool single_segment_flag;
} frame_header_t;

typedef struct {
    frame_header_t header;
    size_t current_total_output;

    const mram(u8 *) dict_content;
    size_t dict_content_len;

    HUF_dtable_t literals_dtable;
    FSE_dtable_t ll_dtable;
    FSE_dtable_t ml_dtable;
    FSE_dtable_t of_dtable;

    u32 previous_offsets[3];
} frame_context_t;

typedef struct {
    FSE_dtable_t ll_table;
    FSE_dtable_t of_table;
    FSE_dtable_t ml_table;

    u16 ll_state;
    u16 of_state;
    u16 ml_state;
} sequence_states_t;

typedef enum {
    seq_literal_length = 0,
    seq_offset = 1,
    seq_match_length = 2,
} seq_part_t;

typedef enum {
    seq_predefined = 0,
    seq_rle = 1,
    seq_fse = 2,
    seq_repeat = 3,
} seq_mode_t;

typedef struct {
    u32 literal_length;
    u32 match_length;
    u32 offset;
} sequence_command_t;

static const i16 SEQ_LITERAL_LENGTH_DEFAULT_DIST[36]
    = { 4, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 1, 1, 1, 1, 1, -1, -1, -1, -1 };
static const i16 SEQ_OFFSET_DEFAULT_DIST[29]
    = { 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -1, -1, -1 };
static const i16 SEQ_MATCH_LENGTH_DEFAULT_DIST[53] = { 1, 4, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1 };

static const u32 SEQ_LITERAL_LENGTH_BASELINES[36] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 20, 22, 24,
    28, 32, 40, 48, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536 };
static const u8 SEQ_LITERAL_LENGTH_EXTRA_BITS[36]
    = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };

static const u32 SEQ_MATCH_LENGTH_BASELINES[53]
    = { 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
          35, 37, 39, 41, 43, 47, 51, 59, 67, 83, 99, 131, 259, 515, 1027, 2051, 4099, 8195, 16387, 32771, 65539 };
static const u8 SEQ_MATCH_LENGTH_EXTRA_BITS[53] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };

#if USE_DEF_GUARDS
static const u8 SEQ_MAX_CODES[3] = { 35, (u8)-1, 52 };
#endif

#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) < (b) ? (a) : (b))

static inline i32 highest_set_bit(const u64 num)
{
#if USE_DEF_GUARDS
    if (unlikely(num == 0)) {
        return -1;
    }
#endif

    return 63 - __builtin_clzl(num);
}

static inline const mram(u8 *) MRAM_get_read_ptr(mram_istream_t *const in, size_t len)
{
#if USE_DEF_GUARDS
    if (unlikely(len > in->len)) {
        ERROR(IN_TOO_SMALL);
    }
    if (unlikely(in->bit_offset != 0)) {
        ERROR(IN_NOT_ALIGNED);
    }
#endif
    const mram(u8 *const) ptr = in->ptr;
    in->ptr += len;
    in->len -= len;
    return ptr;
}
static inline mram(u8 *) MRAM_get_write_ptr(mram_ostream_t *const out, size_t len)
{
#if USE_DEF_GUARDS
    if (unlikely(len > out->len)) {
        ERROR(OUT_TOO_SMALL);
    }
#endif
    mram(u8 *const) ptr = out->ptr;
    out->ptr += len;
    out->len -= len;
    return ptr;
}
static inline mram_ostream_t MRAM_make_ostream(mram(u8 *) out, size_t len) { return (mram_ostream_t) { out, len }; }
static inline mram_istream_t MRAM_make_istream(mram(const u8 *) in, size_t len) { return (mram_istream_t) { in, len, 0 }; }
static inline mram_istream_t MRAM_make_sub_istream(mram_istream_t *const in, size_t len)
{
    const mram(u8 *const) ptr = MRAM_get_read_ptr(in, len);
    return MRAM_make_istream(ptr, len);
}
static inline size_t MRAM_istream_len(const mram_istream_t *const in) { return in->len; }
static inline u64 read_bits_LE(const mram(u8 *) src, const i32 num_bits, const size_t offset)
{
#if USE_DEF_GUARDS
    if (unlikely(num_bits > 64)) {
        ERROR(CORRUPTION);
    }
#endif

    // TODO remove softcache
    src += offset / 8;
    size_t bit_offset = offset % 8;
    u64 res = 0;

    int shift = 0;
    int left = num_bits;

    if (left == 0) {
        return 0;
    }

    if (bit_offset != 0) {
        u8 byte_value = *src++ >> bit_offset;

        if (left < 8) {
            res = byte_value & ((1 << left) - 1);
        } else {
            res = byte_value;
        }

        shift += 8 - bit_offset;
        left -= 8 - bit_offset;
    }

    while (left >= 8) {
        res |= ((u64)((*src++) & 0xff)) << shift;
        shift += 8;
        left -= 8;
    }

    if (left > 0) {
        u64 mask = (1 << left) - 1;
        res += ((u64)((*src++) & mask)) << shift;
    }

    return res;
}
static inline u64 MRAM_read_bits(mram_istream_t *const in, const i32 num_bits)
{
    const size_t full_bytes = (num_bits + in->bit_offset) / 8;
#if USE_DEF_GUARDS
    const size_t bytes = (num_bits + in->bit_offset + 7) / 8;
    if (unlikely(bytes > in->len)) {
        ERROR(IN_TOO_SMALL);
    }
#endif
    const u64 result = read_bits_LE(in->ptr, num_bits, in->bit_offset);
    in->bit_offset = (num_bits + in->bit_offset) % 8;
    in->ptr += full_bytes;
    in->len -= full_bytes;
    return result;
}
static inline void MRAM_align_stream(mram_istream_t *const in)
{
    if (in->bit_offset != 0) {
#if USE_DEF_GUARDS
        if (unlikely(in->len == 0)) {
            ERROR(IN_TOO_SMALL);
        }
#endif
        in->ptr++;
        in->len--;
        in->bit_offset = 0;
    }
}
static inline void MRAM_rewind_bits(mram_istream_t *const in, const u8 num_bits)
{
    const i32 new_offset = in->bit_offset - num_bits;
    const i32 bytes = -(new_offset - 7) / 8;

    in->ptr -= bytes;
    in->len += bytes;
    in->bit_offset = ((new_offset % 8) + 8) % 8;
}
static inline void MRAM_write_byte(mram_ostream_t *const out, u8 symb)
{
#if USE_DEF_GUARDS
    if (unlikely(out->len == 0)) {
        ERROR(OUT_TOO_SMALL);
    }
#endif

    // TODO remove softcache
    out->ptr[0] = symb;
    out->ptr++;
    out->len--;
}
static inline u8 MRAM_read_byte(mram_istream_t *const in)
{
    // TODO remove softcache
    const mram(u8 *) src = MRAM_get_read_ptr(in, 1);
    return src[0];
}
static inline void MRAM_copy(mram_ostream_t *const out, mram_istream_t *const in, size_t len)
{
#if USE_DEF_GUARDS
    if (unlikely(in->bit_offset != 0)) {
        ERROR(IN_NOT_ALIGNED);
    }
    if (unlikely(in->len < len)) {
        ERROR(IN_TOO_SMALL);
    }
    if (unlikely(out->len < len)) {
        ERROR(OUT_TOO_SMALL);
    }
#endif

    u8 *input_cache = in_cache[me()];
    u8 *output_cache = out_cache[me()];

    const mram(u8 *const) input = in->ptr;
    mram(u8 *) output = out->ptr;
    u8 input_offset = ((uintptr_t)input) & (MRAM_CACHE_SIZE - 1);
    u8 output_offset = ((uintptr_t)output) & (MRAM_CACHE_SIZE - 1);
    u32 input_idx = 0;
    u32 output_idx = 0;
    size_t remaining = len;

    if (output_offset != 0) {
        mram_read(output, output_cache, MRAM_CACHE_SIZE);
    }

    mram_read(input, input_cache, MRAM_CACHE_SIZE);
    input_idx += MRAM_CACHE_SIZE;

    while (remaining != 0) {
        if (output_offset > input_offset) {
            size_t part = MIN(remaining, MRAM_CACHE_SIZE - output_offset);
            memcpy(output_cache + output_offset, input_cache + input_offset, part);
            mram_write(output_cache, output + output_idx, MRAM_CACHE_SIZE);

            remaining -= part;
            output_idx += MRAM_CACHE_SIZE;
            input_offset += part;
            output_offset = 0;
        } else if (output_offset < input_offset) {
            size_t part = MIN(remaining, MRAM_CACHE_SIZE - input_offset);
            memcpy(output_cache + output_offset, input_cache + input_offset, part);
            mram_read(input + input_idx, input_cache, MRAM_CACHE_SIZE);

            input_idx += MRAM_CACHE_SIZE;
            output_offset += part;
            input_offset = 0;
        } else {
            // TODO (optimization) we should handle this case outside the while loop
            size_t part = MIN(remaining, MRAM_CACHE_SIZE - input_offset);
            memcpy(output_cache + output_offset, input_cache + input_offset, part);
            mram_write(output_cache, output + output_idx, MRAM_CACHE_SIZE);
            mram_read(input + input_idx, input_cache, MRAM_CACHE_SIZE);

            remaining -= part;
            output_idx += MRAM_CACHE_SIZE;
            input_idx += MRAM_CACHE_SIZE;
            output_offset = 0;
            input_offset = 0;
        }
    }

    in->ptr += len;
    in->len -= len;
    out->ptr += len;
    out->len -= len;
}
static inline void MRAM_memset(mram_ostream_t *const out, u8 value, size_t len)
{
#if USE_DEF_GUARDS
    if (unlikely(out->len < len)) {
        ERROR(OUT_TOO_SMALL);
    }
#endif

    mram(u8 *) ptr = out->ptr;
    u8 *cache = out_cache[me()];
    u32 offset = ((uintptr_t)ptr) & (MRAM_CACHE_SIZE - 1);
    u32 idx = 0;
    size_t remaining = len;
    if (offset != 0) {
        mram_read(ptr, cache, MRAM_CACHE_SIZE);
        size_t part = MIN(MRAM_CACHE_SIZE - offset, len);
        memset(cache + offset, value, part);
        remaining -= part;
        mram_write(cache, ptr, MRAM_CACHE_SIZE);
        idx += part;
    }

    if (remaining >= MRAM_CACHE_SIZE) {
        memset(cache, value, (offset == 0) ? MRAM_CACHE_SIZE : offset);
        do {
            mram_write(cache, ptr + idx, MRAM_CACHE_SIZE);
            idx += MRAM_CACHE_SIZE;
            remaining -= MRAM_CACHE_SIZE;
        } while (remaining >= MRAM_CACHE_SIZE);
    }

    if (remaining > 0) {
        // TODO we may only need to write the cache here without read & memset
        // if the overriden data is not yet important
        mram_read(ptr + idx, cache, MRAM_CACHE_SIZE);
        memset(cache, value, remaining);
        mram_write(cache, ptr + idx, MRAM_CACHE_SIZE);
    }

    out->ptr += len;
    out->len -= len;
}

static void decode_frame(mram_ostream_t *const out, mram_istream_t *const in, const dictionary_t *const dict);
static void init_frame_context(frame_context_t *const ctx, mram_istream_t *const in, const dictionary_t *const dict);
static void parse_frame_header(frame_header_t *const header, mram_istream_t *const in);
static void frame_context_apply_dict(frame_context_t *const ctx, const dictionary_t *const dict);
static void decompress_data(frame_context_t *const ctx, mram_ostream_t *const out, mram_istream_t *const in);
static void decompress_block(frame_context_t *const ctx, mram_ostream_t *const out, mram_istream_t *const in);
static size_t decode_literals(frame_context_t *const ctx, mram_istream_t *const in, mram(u8 *) literals);
static size_t decode_literals_simple(
    mram_istream_t *const in, mram(u8 *const) literals, const block_type_t block_type, const u8 size_format);
static size_t decode_literals_compressed(frame_context_t *const ctx, mram_istream_t *const in, mram(u8 *const) literals,
    const block_type_t block_type, const u8 size_format);
static void decode_huf_table(HUF_dtable_t *const dtable, mram_istream_t *const in);
static void fse_decode_hufweights(u8 *weights, mram_istream_t *const in, u8 *const num_symbs);
static void FSE_decode_header(FSE_dtable_t *const dtable, mram_istream_t *const in, const u8 max_accuracy_log, const u8 fse_table_idx);
static void FSE_init_dtable(FSE_dtable_t *const dtable, const i16 *const norm_freqs, const u8 num_symbs, const u8 accuracy_log, const u8 fse_table_idx);
static void FSE_init_dtable_rle(FSE_dtable_t *const dtable, const u8 symb, const u8 fse_table_idx);
static size_t FSE_decompress_interleaved2(const FSE_dtable_t *const dtable, u8 *const out, mram_istream_t *const in);
static inline void FSE_init_state(
    const FSE_dtable_t *const dtable, u16 *const state, const mram(u8 *const) src, i32 *const offset);
static inline void FSE_update_state(
    const FSE_dtable_t *const dtable, u16 *const state, const mram(u8 *const) src, i32 *const offset);
static inline u8 FSE_peek_symbol(const FSE_dtable_t *const dtable, const u16 state);
static inline u8 FSE_decode_symbol(
    const FSE_dtable_t *const dtable, u16 *const state, const mram(u8 *const) src, i32 *const offset);
static void HUF_init_dtable_usingweights(HUF_dtable_t *const table, u8 *const weights, const i32 num_symbs);
static void HUF_init_dtable(HUF_dtable_t *const table, const u8 *const bits, const i32 num_symbs);
static size_t HUF_decompress_1stream(const HUF_dtable_t *const dtable, mram_ostream_t *const out, mram_istream_t *const in);
static inline void HUF_init_state(
    const HUF_dtable_t *const dtable, u16 *const state, const mram(u8 *const) src, i32 *const offset);
static inline u8 HUF_decode_symbol(
    const HUF_dtable_t *const dtable, u16 *const state, const mram(u8 *const) src, i32 *const offset);
static inline u64 STREAM_read_bits(const mram(u8 *const) src, const i32 bits, i32 *const offset);
static size_t HUF_decompress_4stream(const HUF_dtable_t *const dtable, mram_ostream_t *const out, mram_istream_t *const in);
static size_t decode_num_sequences(mram_istream_t *in);
static const mram(u8 *)
    init_sequences(frame_context_t *const ctx, mram_istream_t *in, sequence_states_t *states, i32 *bit_offset);
static void decode_seq_table(FSE_dtable_t *const table, mram_istream_t *const in, const seq_part_t type, const seq_mode_t mode, const u8 fse_table_idx);
static void decode_sequence(
    sequence_states_t *const states, const mram(u8 *const) src, i32 *const offset, sequence_command_t *const seq);
static void execute_sequence(frame_context_t *const ctx, mram_ostream_t *const out, mram_istream_t *litstream,
    const sequence_command_t *const sequence, u32 *const offset_hist, size_t *total_output);
static u32 copy_literals(const size_t literal_length, mram_istream_t *litstream, mram_ostream_t *const out);
static size_t compute_offset(sequence_command_t seq, u32 *const offset_hist);
static void execute_match_copy(
    frame_context_t *const ctx, size_t offset, size_t match_length, size_t total_output, mram_ostream_t *const out);

dictionary_t dict = {};

size_t decompress(
    mram(void *const) dst, const size_t dst_len, const mram(void *const) src, const size_t src_len)
{
    mram_istream_t in = MRAM_make_istream(src, src_len);
    mram_ostream_t out = MRAM_make_ostream(dst, dst_len);

    decode_frame(&out, &in, &dict);

    return (size_t)(out.ptr - (mram(u8 *))dst);
}

static void decode_frame(mram_ostream_t *const out, mram_istream_t *const in, const dictionary_t *const dict)
{
    const u32 magic_number = (u32)MRAM_read_bits(in, 32);

#if USE_DEF_GUARDS
    if (unlikely(magic_number != ZSTD_MAGIC_NUMBER)) {
        ERROR(INVALID_MAGIC);
    }
#else
    (void) magic_number;
#endif

    frame_context_t ctx;
    init_frame_context(&ctx, in, dict);

#if USE_DEF_GUARDS
    if (unlikely(ctx.header.frame_content_size != 0 && ctx.header.frame_content_size > out->len)) {
        ERROR(OUT_TOO_SMALL);
    }
#endif

    decompress_data(&ctx, out, in);
}

static void init_frame_context(frame_context_t *const ctx, mram_istream_t *const in, const dictionary_t *const dict)
{
    memset(ctx, 0, sizeof(*ctx));

    parse_frame_header(&ctx->header, in);

    ctx->previous_offsets[0] = 1;
    ctx->previous_offsets[1] = 4;
    ctx->previous_offsets[2] = 8;

    frame_context_apply_dict(ctx, dict);
}

static void parse_frame_header(frame_header_t *const header, mram_istream_t *const in)
{
    const u8 descriptor = (u8)MRAM_read_bits(in, 8);

    const u8 frame_content_size_flag = descriptor >> 6;
    const u8 single_segment_flag = (descriptor >> 5) & 1;
    const u8 content_checksum_flag = (descriptor >> 2) & 1;
    const u8 dictionary_id_flag = descriptor & 3;

#if USE_DEF_GUARDS
    const u8 reserved_bit = (descriptor >> 3) & 1;
    if (unlikely(reserved_bit != 0)) {
        ERROR(CORRUPTION);
    }
#endif

    header->single_segment_flag = single_segment_flag != 0;
    header->content_checksum_flag = content_checksum_flag != 0;

    if (!single_segment_flag) {
        u8 window_descriptor = (u8)MRAM_read_bits(in, 8);
        u8 exponent = window_descriptor >> 3;
        u8 mantissa = window_descriptor & 7;

        size_t window_base = (size_t)1 << (10 + exponent);
        size_t window_add = (window_base / 8) * mantissa;
        header->window_size = window_base + window_add;
    }

    if (dictionary_id_flag) {
        const u8 bytes_array[] = { 0, 1, 2, 4 };
        const u8 bytes = bytes_array[dictionary_id_flag];
        header->dictionary_id = (u32)MRAM_read_bits(in, bytes * 8);
    } else {
        header->dictionary_id = 0;
    }

    if (single_segment_flag || frame_content_size_flag) {
        const u8 bytes_array[] = { 1, 2, 4, 8 };
        const u8 bytes = bytes_array[frame_content_size_flag];
        header->frame_content_size = MRAM_read_bits(in, bytes * 8);
        if (bytes == 2) {
            header->frame_content_size += 256;
        }
    } else {
        header->frame_content_size = 0;
    }

    if (single_segment_flag) {
        header->window_size = header->frame_content_size;
    }
}

static void frame_context_apply_dict(frame_context_t *const ctx, const dictionary_t *const dict)
{
    if (!dict || !dict->content) {
        return;
    }

#if USE_DEF_GUARDS
    if (unlikely(ctx->header.dictionary_id != 0 && ctx->header.dictionary_id != dict->dictionary_id)) {
        ERROR(WRONG_DICTIONARY);
    }
#endif

    ctx->dict_content = dict->content;
    ctx->dict_content_len = dict->content_size;

    if (dict->dictionary_id != 0) {
        // TODO Tables

        memcpy(ctx->previous_offsets, dict->previous_offsets, sizeof(ctx->previous_offsets));
    }
}

static void decompress_data(frame_context_t *const ctx, mram_ostream_t *const out, mram_istream_t *const in)
{
    bool last_block;

    do {
        last_block = ((u32)MRAM_read_bits(in, 1)) != 0;
        const block_type_t block_type = (block_type_t)MRAM_read_bits(in, 2);
        const size_t block_len = MRAM_read_bits(in, 21);

        switch (block_type) {
        case RAW_BLOCK: {
            MRAM_copy(out, in, block_len);
            ctx->current_total_output += block_len;
            break;
        }
        case RLE_BLOCK: {
            u8 value = MRAM_read_byte(in);
            MRAM_memset(out, value, block_len);
            ctx->current_total_output += block_len;
            break;
        }
        case COMPRESSED_BLOCK: {
            mram_istream_t block_stream = MRAM_make_sub_istream(in, block_len);
            decompress_block(ctx, out, &block_stream);
            break;
        }
        case RESERVED_BLOCK: {
            ERROR(CORRUPTION);
            break;
        }
        default: {
            ERROR(IMPOSSIBLE);
            break;
        }
        }
    } while (!last_block);
}

static void decompress_block(frame_context_t *const ctx, mram_ostream_t *const out, mram_istream_t *const in)
{
    mram(u8 *) literals = literals_storage[me()];
    const size_t literals_size = decode_literals(ctx, in, literals);
    mram_istream_t litstream = MRAM_make_istream(literals, literals_size);

    const size_t num_sequences = decode_num_sequences(in);

    sequence_states_t states;
    i32 bit_offset;
    const mram(u8 *const) src = init_sequences(ctx, in, &states, &bit_offset);

    u32 *const offset_hist = ctx->previous_offsets;
    size_t total_output = ctx->current_total_output;

    for (size_t each_sequence = 0; each_sequence < num_sequences; ++each_sequence) {
        sequence_command_t sequence;
        decode_sequence(&states, src, &bit_offset, &sequence);
        execute_sequence(ctx, out, &litstream, &sequence, offset_hist, &total_output);
    }

    size_t len = MRAM_istream_len(&litstream);
    copy_literals(len, &litstream, out);
    total_output += len;

    ctx->current_total_output = total_output;
}

static size_t decode_literals(frame_context_t *const ctx, mram_istream_t *const in, mram(u8 *const) literals)
{
    const u8 block_type = (u8)MRAM_read_bits(in, 2);
    const u8 size_format = (u8)MRAM_read_bits(in, 2);

    if (block_type <= 1) {
        return decode_literals_simple(in, literals, block_type, size_format);
    } else {
        return decode_literals_compressed(ctx, in, literals, block_type, size_format);
    }
}

static size_t decode_literals_simple(
    mram_istream_t *const in, mram(u8 *const) literals, const block_type_t block_type, const u8 size_format)
{
    size_t size;

    switch (size_format) {
    case 0:
    case 2:
        MRAM_rewind_bits(in, 1);
        size = MRAM_read_bits(in, 5);
        break;
    case 1:
        size = MRAM_read_bits(in, 12);
    case 3:
        size = MRAM_read_bits(in, 20);
    default:
        ERROR(IMPOSSIBLE);
    }

#if USE_DEF_GUARDS
    if (unlikely(size > MAX_LITERALS_SIZE)) {
        ERROR(CORRUPTION);
    }
#endif

    mram_ostream_t litstream = MRAM_make_ostream(literals, size);
    switch (block_type) {
    case RAW_BLOCK: {
        // TODO literals is READ-ONLY (I think), we may only need to redirect literals to in
        MRAM_copy(&litstream, in, size);
        break;
    }
    case RLE_BLOCK: {
        const u8 value = MRAM_read_byte(in);
        MRAM_memset(&litstream, value, size);
        break;
    }
    default:
        ERROR(IMPOSSIBLE);
    }

    return size;
}

static size_t decode_literals_compressed(frame_context_t *const ctx, mram_istream_t *const in, mram(u8 *const) literals,
    const block_type_t block_type, const u8 size_format)
{
    size_t regenerated_size, compressed_size;
    u8 num_streams = 4;

    switch (size_format) {
    case 0:
        num_streams = 1;
    case 1:
        regenerated_size = MRAM_read_bits(in, 10);
        compressed_size = MRAM_read_bits(in, 10);
        break;
    case 2:
        regenerated_size = MRAM_read_bits(in, 14);
        compressed_size = MRAM_read_bits(in, 14);
        break;
    case 3:
        regenerated_size = MRAM_read_bits(in, 18);
        compressed_size = MRAM_read_bits(in, 18);
        break;
    default:
        ERROR(IMPOSSIBLE);
    }

#if USE_DEF_GUARDS
    if (unlikely(regenerated_size > MAX_LITERALS_SIZE)) {
        ERROR(CORRUPTION);
    }
#endif

    mram_ostream_t lit_stream = MRAM_make_ostream(literals, regenerated_size);
    mram_istream_t huf_stream = MRAM_make_sub_istream(in, compressed_size);

    if (block_type == COMPRESSED_BLOCK) {
        decode_huf_table(&ctx->literals_dtable, &huf_stream);
    } else {
#if USE_DEF_GUARDS
        if (unlikely(ctx->literals_dtable.symbols == NULL)) {
            ERROR(CORRUPTION);
        }
#endif
    }

    size_t symbols_decoded;
    if (num_streams == 1) {
        symbols_decoded = HUF_decompress_1stream(&ctx->literals_dtable, &lit_stream, &huf_stream);
    } else {
        symbols_decoded = HUF_decompress_4stream(&ctx->literals_dtable, &lit_stream, &huf_stream);
    }

#if USE_DEF_GUARDS
    if (unlikely(symbols_decoded != regenerated_size)) {
        ERROR(CORRUPTION);
    }
#endif

    return regenerated_size;
}

static void decode_huf_table(HUF_dtable_t *const dtable, mram_istream_t *const in)
{
    u8 weights[HUF_MAX_SYMBOLS];
    memset(weights, 0, sizeof(weights));
    const u8 header = MRAM_read_bits(in, 8);
    u8 num_symbs;

    if (header >= 128) {
        num_symbs = header - 127;
        const size_t bytes = (num_symbs + 1) / 2;
        const mram(u8 *const) weight_src = MRAM_get_read_ptr(in, bytes);

        for (u8 each_symb = 0; each_symb < num_symbs; ++each_symb) {
            const u8 value = weight_src[each_symb / 2];
            if ((each_symb % 2) == 0) {
                weights[each_symb] = value >> 4;
            } else {
                weights[each_symb] = value & 0xf;
            }
        }
    } else {
        mram_istream_t fse_stream = MRAM_make_sub_istream(in, header);
        fse_decode_hufweights(weights, &fse_stream, &num_symbs);
    }

    HUF_init_dtable_usingweights(dtable, weights, num_symbs);
}

static void fse_decode_hufweights(u8 *weights, mram_istream_t *const in, u8 *const num_symbs)
{
    FSE_dtable_t dtable;
    FSE_decode_header(&dtable, in, FSE_USED_ACCURACY_LOG, FSE_LITERALS_TABLE_IDX);
    *num_symbs = FSE_decompress_interleaved2(&dtable, weights, in);
}

static void FSE_decode_header(FSE_dtable_t *const dtable, mram_istream_t *const in, const u8 max_accuracy_log, const u8 fse_table_idx)
{
#if USE_DEF_GUARDS
    if (unlikely(max_accuracy_log > FSE_MAX_ACCURACY_LOG)) {
        ERROR(FSE_ACCURACY_TOO_LARGE);
    }
#else
    (void) max_accuracy_log;
#endif
    const u8 accuracy_log = 5 + MRAM_read_bits(in, 4);
#if USE_DEF_GUARDS
    if (unlikely(accuracy_log > max_accuracy_log)) {
        ERROR(FSE_ACCURACY_TOO_LARGE);
    }
#endif

    i32 remaining = 1 << accuracy_log;
    i16 frequencies[FSE_MAX_SYMBS];

    i32 symb = 0;
    while (remaining > 0 && symb < FSE_MAX_SYMBS) {
        const u8 num_bits = highest_set_bit(remaining + 1) + 1;
        u16 val = MRAM_read_bits(in, num_bits);
        const u16 lower_mask = ((u16)1 << (num_bits - 1)) - 1;
        const u16 threshold = ((u16)1 << num_bits) - 1 - (remaining + 1);

        if ((val & lower_mask) < threshold) {
            MRAM_rewind_bits(in, 1);
            val = val & lower_mask;
        } else if (val > lower_mask) {
            val = val - threshold;
        }

        const i16 proba = (i16)val - 1;
        remaining -= proba < 0 ? -proba : proba;
        frequencies[symb++] = proba;

        if (proba == 0) {
            u8 repeat;
            do {
                repeat = MRAM_read_bits(in, 2);

                for (u8 i = 0; i < repeat && symb < FSE_MAX_SYMBS; ++i) {
                    frequencies[symb++] = 0;
                }
            } while (repeat == 3);
        }
    }
    MRAM_align_stream(in);

#if USE_DEF_GUARDS
    if (unlikely(remaining != 0)) {
        ERROR(CORRUPTION);
    }
#endif

    FSE_init_dtable(dtable, frequencies, symb, accuracy_log, fse_table_idx);
}

static void FSE_init_dtable(FSE_dtable_t *const dtable, const i16 *const norm_freqs, const u8 num_symbs, const u8 accuracy_log, const u8 fse_table_idx)
{
    dtable->accuracy_log = accuracy_log;
    // TODO remove softcache
    dtable->symbols = fse_symbols[me()][fse_table_idx];
    dtable->num_bits = fse_num_bits[me()][fse_table_idx];
    dtable->new_state_base = fse_new_state_base[me()][fse_table_idx];

    u16 state_desc[FSE_MAX_SYMBS];
    const size_t size = (size_t)1 << accuracy_log;
    i32 high_threshold = size;
    for (u8 s = 0; s < num_symbs; ++s) {
        if (norm_freqs[s] == -1) {
            dtable->symbols[--high_threshold] = s;
            state_desc[s] = 1;
        }
    }

    const u16 step = (size >> 1) + (size >> 3) + 3;
    const u16 mask = size - 1;
    u16 pos = 0;
    for (u8 s = 0; s < num_symbs; ++s) {
        if (norm_freqs[s] <= 0) {
            continue;
        }

        state_desc[s] = norm_freqs[s];

        for (u16 i = 0; i < norm_freqs[s]; ++i) {
            dtable->symbols[pos] = s;

            do {
                pos = (pos + step) & mask;
            } while (pos >= high_threshold);
        }
    }
#if USE_DEF_GUARDS
    if (unlikely(pos != 0)) {
        ERROR(CORRUPTION);
    }
#endif

    for (size_t i = 0; i < size; ++i) {
        u8 symbol = dtable->symbols[i];
        u16 next_state_desc = state_desc[symbol]++;
        dtable->num_bits[i] = (u8)(accuracy_log - highest_set_bit(next_state_desc));
        dtable->new_state_base[i] = ((u16)next_state_desc << dtable->num_bits[i]) - size;
    }
}

static void FSE_init_dtable_rle(FSE_dtable_t *const dtable, const u8 symb, const u8 fse_table_idx)
{
    // TODO could be done in WRAM
    dtable->symbols = fse_symbols[me()][fse_table_idx];
    dtable->num_bits = fse_num_bits[me()][fse_table_idx];
    dtable->new_state_base = fse_new_state_base[me()][fse_table_idx];

    dtable->symbols[0] = symb;
    dtable->num_bits[0] = 0;
    dtable->new_state_base[0] = 0;
    dtable->accuracy_log = 0;
}

static size_t FSE_decompress_interleaved2(const FSE_dtable_t *const dtable, u8 *const out, mram_istream_t *const in)
{
    const size_t len = MRAM_istream_len(in);
#if USE_DEF_GUARDS
    if (unlikely(len == 0)) {
        ERROR(IN_TOO_SMALL);
    }
#endif

    // todo remove softcache
    const mram(u8 *const) src = MRAM_get_read_ptr(in, len);
    const u32 padding = 8 - highest_set_bit(src[len - 1]);
    i32 offset = len * 8 - padding;

    u16 state1, state2;
    FSE_init_state(dtable, &state1, src, &offset);
    FSE_init_state(dtable, &state2, src, &offset);

    size_t symbols_written = 0;

    while (true) {
        out[symbols_written++] = FSE_decode_symbol(dtable, &state1, src, &offset);
        if (offset < 0) {
            out[symbols_written++] = FSE_peek_symbol(dtable, state2);
            break;
        }

        out[symbols_written++] = FSE_decode_symbol(dtable, &state2, src, &offset);
        if (offset < 0) {
            out[symbols_written++] = FSE_peek_symbol(dtable, state1);
            break;
        }
    }

    return symbols_written;
}

static inline void FSE_init_state(
    const FSE_dtable_t *const dtable, u16 *const state, const mram(u8 *const) src, i32 *const offset)
{
    const u8 bits = dtable->accuracy_log;
    *state = STREAM_read_bits(src, bits, offset);
}

static inline void FSE_update_state(
    const FSE_dtable_t *const dtable, u16 *const state, const mram(u8 *const) src, i32 *const offset)
{
    // TODO remove softcache
    const u8 bits = dtable->num_bits[*state];
    const u16 rest = STREAM_read_bits(src, bits, offset);
    *state = dtable->new_state_base[*state] + rest;
}

static inline u8 FSE_peek_symbol(const FSE_dtable_t *const dtable, const u16 state)
{
    // TODO remove softcache
    return dtable->symbols[state];
}

static inline u8 FSE_decode_symbol(
    const FSE_dtable_t *const dtable, u16 *const state, const mram(u8 *const) src, i32 *const offset)
{
    const u8 symb = FSE_peek_symbol(dtable, *state);
    FSE_update_state(dtable, state, src, offset);
    return symb;
}

static void HUF_init_dtable_usingweights(HUF_dtable_t *const table, u8 *const weights, const i32 num_symbs)
{
#if USE_DEF_GUARDS
    if (unlikely(num_symbs + 1 > HUF_MAX_SYMBOLS)) {
        ERROR(HUF_TOO_MANY_SYMBS);
    }
#endif

    u8 bits[HUF_MAX_SYMBOLS];
    u64 weight_sum = 0;
    for (i32 i = 0; i < num_symbs; ++i) {
#if USE_DEF_GUARDS
        if (unlikely(weights[i] > HUF_MAX_BITS)) {
            ERROR(CORRUPTION);
        }
#endif
        weight_sum += weights[i] > 0 ? (u64)1 << (weights[i] - 1) : 0;
    }

    const u32 max_bits = highest_set_bit(weight_sum) + 1;
    const u64 left_over = ((u64)1 << max_bits) - weight_sum;

#if USE_DEF_GUARDS
    if (unlikely(left_over & (left_over - 1))) {
        ERROR(CORRUPTION);
    }
#endif

    const u32 last_weight = highest_set_bit(left_over) + 1;
    for (i32 i = 0; i < num_symbs; ++i) {
        bits[i] = weights[i] > 0 ? (max_bits + 1 - weights[i]) : 0;
    }
    bits[num_symbs] = max_bits + 1 - last_weight;

    HUF_init_dtable(table, bits, num_symbs + 1);
}

static void HUF_init_dtable(HUF_dtable_t *const table, const u8 *const bits, const i32 num_symbs)
{
    memset(table, 0, sizeof(HUF_dtable_t));
#if USE_DEF_GUARDS
    if (unlikely(num_symbs > HUF_MAX_SYMBOLS)) {
        ERROR(HUF_TOO_MANY_SYMBS);
    }
#endif

    u8 max_bits = 0;
    u16 rank_count[HUF_MAX_BITS + 1];
    memset(rank_count, 0, sizeof(rank_count));
    for (i32 i = 0; i < num_symbs; ++i) {
#if USE_DEF_GUARDS
        if (unlikely(bits[i] > HUF_MAX_BITS)) {
            ERROR(HUF_TABLE_DEPTH_TOO_LARGE);
        }
#endif

        max_bits = MAX(max_bits, bits[i]);
        rank_count[bits[i]]++;
    }

    table->max_bits = max_bits;
    table->symbols = huf_symbols[me()];
    table->num_bits = huf_num_bits[me()];

    u32 rank_idx[HUF_MAX_BITS + 1];
    rank_idx[max_bits] = 0;

    for (i32 i = max_bits; i >= 1; --i) {
        rank_idx[i - 1] = rank_idx[i] + rank_count[i] * (1 << (max_bits - i));
        u16 len = rank_idx[i - 1] - rank_idx[i];
        mram_ostream_t numbitsstream = MRAM_make_ostream(&table->num_bits[rank_idx[i]], len);
        MRAM_memset(&numbitsstream, i, len);
    }

#if USE_DEF_GUARDS
    const size_t table_size = 1 << max_bits;
    if (unlikely(rank_idx[0] != table_size)) {
        ERROR(CORRUPTION);
    }
#endif

    for (i32 i = 0; i < num_symbs; ++i) {
        if (bits[i] != 0) {
            const u16 code = rank_idx[bits[i]];
            const u16 len = 1 << (max_bits - bits[i]);
            mram_ostream_t symstream = MRAM_make_ostream(&table->symbols[code], len);
            MRAM_memset(&symstream, i, len);
            rank_idx[bits[i]] += len;
        }
    }
}

static size_t HUF_decompress_1stream(const HUF_dtable_t *const dtable, mram_ostream_t *const out, mram_istream_t *const in)
{
    const size_t len = MRAM_istream_len(in);
#if USE_DEF_GUARDS
    if (unlikely(len == 0)) {
        ERROR(IN_TOO_SMALL);
    }
#endif

    // TODO remove softcache
    const mram(u8 *const) src = MRAM_get_read_ptr(in, len);
    const i32 padding = 8 - highest_set_bit(src[len - 1]);
    i32 bit_offset = len * 8 - padding;
    u16 state;

    HUF_init_state(dtable, &state, src, &bit_offset);

    size_t symbols_written = 0;
    while (bit_offset > -dtable->max_bits) {
        MRAM_write_byte(out, HUF_decode_symbol(dtable, &state, src, &bit_offset));
        symbols_written++;
    }

#if USE_DEF_GUARDS
    if (unlikely(bit_offset != -dtable->max_bits)) {
        ERROR(CORRUPTION);
    }
#endif

    return symbols_written;
}

static inline void HUF_init_state(
    const HUF_dtable_t *const dtable, u16 *const state, const mram(u8 *const) src, i32 *const offset)
{
    const u8 bits = dtable->max_bits;
    *state = STREAM_read_bits(src, bits, offset);
}

static inline u8 HUF_decode_symbol(
    const HUF_dtable_t *const dtable, u16 *const state, const mram(u8 *const) src, i32 *const offset)
{
    // TODO remove softcache here?
    const u8 symb = dtable->symbols[*state];
    const u8 bits = dtable->num_bits[*state];
    const u16 rest = STREAM_read_bits(src, bits, offset);
    *state = ((*state << bits) + rest) & (((u16)1 << dtable->max_bits) - 1);
    return symb;
}

static inline u64 STREAM_read_bits(const mram(u8 *const) src, const i32 bits, i32 *const offset)
{
    *offset = *offset - bits;
    size_t actual_off = *offset;
    size_t actual_bits = bits;
    if (*offset < 0) {
        actual_bits += *offset;
        actual_off = 0;
    }
    u64 res = read_bits_LE(src, actual_bits, actual_off);
    if (*offset < 0) {
        res = -*offset >= 64 ? 0 : (res << -*offset);
    }
    return res;
}

static size_t HUF_decompress_4stream(const HUF_dtable_t *const dtable, mram_ostream_t *const out, mram_istream_t *const in)
{
    const size_t csize1 = MRAM_read_bits(in, 16);
    const size_t csize2 = MRAM_read_bits(in, 16);
    const size_t csize3 = MRAM_read_bits(in, 16);

    mram_istream_t in1 = MRAM_make_sub_istream(in, csize1);
    mram_istream_t in2 = MRAM_make_sub_istream(in, csize2);
    mram_istream_t in3 = MRAM_make_sub_istream(in, csize3);
    mram_istream_t in4 = MRAM_make_sub_istream(in, MRAM_istream_len(in));

    size_t total_output = 0;

    total_output += HUF_decompress_1stream(dtable, out, &in1);
    total_output += HUF_decompress_1stream(dtable, out, &in2);
    total_output += HUF_decompress_1stream(dtable, out, &in3);
    total_output += HUF_decompress_1stream(dtable, out, &in4);

    return total_output;
}

static size_t decode_num_sequences(mram_istream_t *in)
{
    u8 header = MRAM_read_bits(in, 8);

    if (header < 128) {
        return header;
    }

    if (header < 255) {
        return ((header - 128) << 8) + MRAM_read_bits(in, 8);
    }

    return MRAM_read_bits(in, 16) + 0x7F00;
}

static const mram(u8 *) init_sequences(frame_context_t *const ctx, mram_istream_t *in, sequence_states_t *states, i32 *bit_offset)
{
    u8 compression_modes = MRAM_read_bits(in, 8);
#if USE_DEF_GUARDS
    if (unlikely((compression_modes & 3) != 0)) {
        ERROR(CORRUPTION);
    }
#endif

    decode_seq_table(&ctx->ll_dtable, in, seq_literal_length, (compression_modes >> 6) & 3, FSE_LL_TABLE_IDX);
    decode_seq_table(&ctx->of_dtable, in, seq_offset, (compression_modes >> 4) & 3, FSE_OF_TABLE_IDX);
    decode_seq_table(&ctx->ml_dtable, in, seq_match_length, (compression_modes >> 2) & 3, FSE_ML_TABLE_IDX);

    states->ll_table = ctx->ll_dtable;
    states->of_table = ctx->of_dtable;
    states->ml_table = ctx->ml_dtable;

    const size_t len = MRAM_istream_len(in);
    // TODO remove softcache
    const mram(u8 *const) src = MRAM_get_read_ptr(in, len);
    const i32 padding = 8 - highest_set_bit(src[len - 1]);
    *bit_offset = (i32)(len * 8 - (size_t)padding);

    FSE_init_state(&states->ll_table, &states->ll_state, src, bit_offset);
    FSE_init_state(&states->of_table, &states->of_state, src, bit_offset);
    FSE_init_state(&states->ml_table, &states->ml_state, src, bit_offset);

    return src;
}

static void decode_seq_table(FSE_dtable_t *const table, mram_istream_t *const in, const seq_part_t type, const seq_mode_t mode, const u8 fse_table_idx)
{
    const i16 *const default_distributions[]
        = { SEQ_LITERAL_LENGTH_DEFAULT_DIST, SEQ_OFFSET_DEFAULT_DIST, SEQ_MATCH_LENGTH_DEFAULT_DIST };
    const size_t default_distributions_lengths[] = { 36, 29, 53 };
    const size_t default_distributions_accuracies[] = { 6, 5, 6 };
    const size_t max_accuracies[] = { 9, 8, 9 };

    if (mode != seq_repeat) {
        memset(table, 0, sizeof(*table));
    }

    switch (mode) {
    case seq_predefined: {
        const i16 *distribution = default_distributions[type];
        const size_t symbs = default_distributions_lengths[type];
        const size_t accuracy_log = default_distributions_accuracies[type];

        FSE_init_dtable(table, distribution, symbs, accuracy_log, fse_table_idx);
        break;
    }
    case seq_rle: {
        const u8 symb = MRAM_read_byte(in);
        FSE_init_dtable_rle(table, symb, fse_table_idx);
        break;
    }
    case seq_fse: {
        FSE_decode_header(table, in, max_accuracies[type], fse_table_idx);
        break;
    }
    case seq_repeat: {
#if USE_DEF_GUARDS
        if (unlikely(!table->symbols)) {
            ERROR(CORRUPTION);
        }
#endif
    }
    default:
        ERROR(IMPOSSIBLE);
        break;
    }
}

static void decode_sequence(
    sequence_states_t *const states, const mram(u8 *const) src, i32 *const offset, sequence_command_t *const seq)
{
    const u8 of_code = FSE_peek_symbol(&states->of_table, states->of_state);
    const u8 ll_code = FSE_peek_symbol(&states->ll_table, states->ll_state);
    const u8 ml_code = FSE_peek_symbol(&states->ml_table, states->ml_state);

#if USE_DEF_GUARDS
    if (unlikely(ll_code > SEQ_MAX_CODES[seq_literal_length] || ml_code > SEQ_MAX_CODES[seq_match_length])) {
        ERROR(CORRUPTION);
    }
#endif

    seq->offset = ((u32)1 << of_code) + STREAM_read_bits(src, of_code, offset);
    seq->match_length = SEQ_MATCH_LENGTH_BASELINES[ml_code] + STREAM_read_bits(src, SEQ_MATCH_LENGTH_EXTRA_BITS[ml_code], offset);
    seq->literal_length
        = SEQ_LITERAL_LENGTH_BASELINES[ll_code] + STREAM_read_bits(src, SEQ_LITERAL_LENGTH_EXTRA_BITS[ll_code], offset);

    if (*offset != 0) {
        FSE_update_state(&states->ll_table, &states->ll_state, src, offset);
        FSE_update_state(&states->ml_table, &states->ml_state, src, offset);
        FSE_update_state(&states->of_table, &states->of_state, src, offset);
    }
}

static void execute_sequence(frame_context_t *const ctx, mram_ostream_t *const out, mram_istream_t *litstream,
    const sequence_command_t *const seq, u32 *const offset_hist, size_t *total_output)
{
    *total_output += copy_literals(seq->literal_length, litstream, out);
    size_t const offset = compute_offset(*seq, offset_hist);
    size_t const match_length = seq->match_length;
    execute_match_copy(ctx, offset, match_length, *total_output, out);
    *total_output += match_length;
}

static u32 copy_literals(const size_t literal_length, mram_istream_t *litstream, mram_ostream_t *const out)
{
#if USE_DEF_GUARDS
    if (unlikely(literal_length > MRAM_istream_len(litstream))) {
        ERROR(CORRUPTION);
    }
#endif

    MRAM_copy(out, litstream, literal_length);

    return literal_length;
}

static size_t compute_offset(sequence_command_t seq, u32 *const offset_hist)
{
    size_t offset;

    if (seq.offset <= 3) {
        u32 idx = seq.offset - 1;

        if (seq.literal_length == 0) {
            idx++;
        }

        if (idx == 0) {
            offset = offset_hist[0];
        } else {
            offset = idx < 3 ? offset_hist[idx] : offset_hist[0] - 1;

            if (idx > 1) {
                offset_hist[2] = offset_hist[1];
            }

            offset_hist[1] = offset_hist[0];
            offset_hist[0] = offset;
        }
    } else {
        offset = seq.offset - 3;

        offset_hist[2] = offset_hist[1];
        offset_hist[1] = offset_hist[0];
        offset_hist[0] = offset;
    }

    return offset;
}

static void execute_match_copy(
    frame_context_t *const ctx, size_t offset, size_t match_length, size_t total_output, mram_ostream_t *const out)
{
    // TODO remove softcache
    mram(u8 *) write_ptr = MRAM_get_write_ptr(out, match_length);
    if (total_output <= ctx->header.window_size) {
#if USE_DEF_GUARDS
        if (unlikely(offset > total_output + ctx->dict_content_len)) {
            ERROR(CORRUPTION);
        }
#endif

        if (offset > total_output) {
            const size_t dict_copy = MIN(offset - total_output, match_length);
            const size_t dict_offset = ctx->dict_content_len - (offset - total_output);

            mram_istream_t instream = MRAM_make_istream(ctx->dict_content + dict_offset, dict_copy);
            mram_ostream_t outstream = MRAM_make_ostream(write_ptr, dict_copy);
            MRAM_copy(&outstream, &instream, dict_copy);
            write_ptr += dict_copy;
            match_length -= dict_copy;
        }
    }
#if USE_DEF_GUARDS
    else if (unlikely(offset > ctx->header.window_size)) {
        ERROR(CORRUPTION);
    }
#endif

    for (size_t j = 0; j < match_length; ++j) {
        *write_ptr = *(write_ptr - offset);
        write_ptr++;
    }
}
