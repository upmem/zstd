#include <attributes.h>
#include <defs.h>
#include <mram.h>
#include <perfcounter.h>
#include <seqread.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define likely(x) __builtin_expect(x, 1)
#define unlikely(x) __builtin_expect(x, 0)

#define ALIGN(x, a)         ALIGN_MASK((x), (a) - 1)
#define ALIGN_MASK(x, mask) (((x) + (mask)) & ~(mask))

#define MIN(a,b) ((a)<(b) ? (a) : (b))
#define MAX(a,b) ((a)>(b) ? (a) : (b))

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t s8;
typedef int16_t s16;
typedef int32_t s32;
typedef int64_t s64;

#define STREAM_ACCUMULATOR_MIN_32  25
#define STREAM_ACCUMULATOR_MIN_64  57

#define ZSTD_MAGIC_NUMBER (0xFD2FB528U)

#define ZSTD_REP_NUM      3                 /* number of repcodes */
static const u32 repStartValue[ZSTD_REP_NUM] = { 1, 4, 8 };

#define ZSTD_BLOCKSIZELOG_MAX  17
#define ZSTD_BLOCKSIZE_MAX     (1<<ZSTD_BLOCKSIZELOG_MAX)

#define ZSTD_WINDOWLOG_ABSOLUTEMIN 10
#define ZSTD_CONTENTSIZE_UNKNOWN (0U - 1)
#define ZSTD_BLOCKHEADERSIZE 3

#define MIN_SEQUENCES_SIZE 1 /* nbSeq==0 */
#define MIN_CBLOCK_SIZE (1 /*litCSize*/ + 1 /* RLE or RAW */ + MIN_SEQUENCES_SIZE /* nbSeq==0 */)   /* for a non-null block */

#define LONGNBSEQ 0x7F00

#define ZSTD_WINDOWLOG_MAX_32    30

/* We need to add at most (ZSTD_WINDOWLOG_MAX_32 - 1) bits to read the maximum
 * offset bits. But we can only read at most (STREAM_ACCUMULATOR_MIN_32 - 1)
 * bits before reloading. This value is the maximum number of bytes we read
 * after reloading when we are decoding long offsets.
 */
#define LONG_OFFSETS_MAX_EXTRA_BITS_32                       \
    (ZSTD_WINDOWLOG_MAX_32 > STREAM_ACCUMULATOR_MIN_32       \
        ? ZSTD_WINDOWLOG_MAX_32 - STREAM_ACCUMULATOR_MIN_32  \
        : 0)

#define MaxML   52
#define MaxLL   35
#define MaxOff  31
#define MaxSeq MAX(MaxLL, MaxML)   /* Assumption : MaxOff < MaxLL,MaxML */

#define MLFSELog    9
#define LLFSELog    9
#define OffFSELog   8
#define MaxFSELog  MAX(MAX(MLFSELog, LLFSELog), OffFSELog)

#define LL_DEFAULTNORMLOG 6
#define ML_DEFAULTNORMLOG 6
#define OF_DEFAULTNORMLOG 5

static const u32 LL_bits[MaxLL+1] = { 0, 0, 0, 0, 0, 0, 0, 0,
                                      0, 0, 0, 0, 0, 0, 0, 0,
                                      1, 1, 1, 1, 2, 2, 3, 3,
                                      4, 6, 7, 8, 9,10,11,12,
                                     13,14,15,16 };

static const u32 OF_bits[MaxOff+1] = {
                     0,  1,  2,  3,  4,  5,  6,  7,
                     8,  9, 10, 11, 12, 13, 14, 15,
                    16, 17, 18, 19, 20, 21, 22, 23,
                    24, 25, 26, 27, 28, 29, 30, 31 };


static const u32 ML_bits[MaxML+1] = { 0, 0, 0, 0, 0, 0, 0, 0,
                                      0, 0, 0, 0, 0, 0, 0, 0,
                                      0, 0, 0, 0, 0, 0, 0, 0,
                                      0, 0, 0, 0, 0, 0, 0, 0,
                                      1, 1, 1, 1, 2, 2, 3, 3,
                                      4, 4, 5, 7, 8, 9,10,11,
                                     12,13,14,15,16 };

static const u32 LL_base[MaxLL+1] = {
                 0,    1,    2,     3,     4,     5,     6,      7,
                 8,    9,   10,    11,    12,    13,    14,     15,
                16,   18,   20,    22,    24,    28,    32,     40,
                48,   64, 0x80, 0x100, 0x200, 0x400, 0x800, 0x1000,
                0x2000, 0x4000, 0x8000, 0x10000 };

static const u32 OF_base[MaxOff+1] = {
                 0,        1,       1,       5,     0xD,     0x1D,     0x3D,     0x7D,
                 0xFD,   0x1FD,   0x3FD,   0x7FD,   0xFFD,   0x1FFD,   0x3FFD,   0x7FFD,
                 0xFFFD, 0x1FFFD, 0x3FFFD, 0x7FFFD, 0xFFFFD, 0x1FFFFD, 0x3FFFFD, 0x7FFFFD,
                 0xFFFFFD, 0x1FFFFFD, 0x3FFFFFD, 0x7FFFFFD, 0xFFFFFFD, 0x1FFFFFFD, 0x3FFFFFFD, 0x7FFFFFFD };

static const u32 ML_base[MaxML+1] = {
                     3,  4,  5,    6,     7,     8,     9,    10,
                    11, 12, 13,   14,    15,    16,    17,    18,
                    19, 20, 21,   22,    23,    24,    25,    26,
                    27, 28, 29,   30,    31,    32,    33,    34,
                    35, 37, 39,   41,    43,    47,    51,    59,
                    67, 83, 99, 0x83, 0x103, 0x203, 0x403, 0x803,
                    0x1003, 0x2003, 0x4003, 0x8003, 0x10003 };

struct SeqSymbolHeader {
    u32 fastMode;
    u32 tableLog;
};

struct SeqSymbol {
    u16 nextState;
    u8  nbAdditionalBits;
    u8  nbBits;
    u32 baseValue;
};

static const __mram struct SeqSymbol LL_defaultDTable[(1<<LL_DEFAULTNORMLOG)+1] = {
     {  1,  1,  1, LL_DEFAULTNORMLOG},  /* header : fastMode, tableLog */
     /* nextState, nbAddBits, nbBits, baseVal */
     {  0,  0,  4,    0},  { 16,  0,  4,    0},
     { 32,  0,  5,    1},  {  0,  0,  5,    3},
     {  0,  0,  5,    4},  {  0,  0,  5,    6},
     {  0,  0,  5,    7},  {  0,  0,  5,    9},
     {  0,  0,  5,   10},  {  0,  0,  5,   12},
     {  0,  0,  6,   14},  {  0,  1,  5,   16},
     {  0,  1,  5,   20},  {  0,  1,  5,   22},
     {  0,  2,  5,   28},  {  0,  3,  5,   32},
     {  0,  4,  5,   48},  { 32,  6,  5,   64},
     {  0,  7,  5,  128},  {  0,  8,  6,  256},
     {  0, 10,  6, 1024},  {  0, 12,  6, 4096},
     { 32,  0,  4,    0},  {  0,  0,  4,    1},
     {  0,  0,  5,    2},  { 32,  0,  5,    4},
     {  0,  0,  5,    5},  { 32,  0,  5,    7},
     {  0,  0,  5,    8},  { 32,  0,  5,   10},
     {  0,  0,  5,   11},  {  0,  0,  6,   13},
     { 32,  1,  5,   16},  {  0,  1,  5,   18},
     { 32,  1,  5,   22},  {  0,  2,  5,   24},
     { 32,  3,  5,   32},  {  0,  3,  5,   40},
     {  0,  6,  4,   64},  { 16,  6,  4,   64},
     { 32,  7,  5,  128},  {  0,  9,  6,  512},
     {  0, 11,  6, 2048},  { 48,  0,  4,    0},
     { 16,  0,  4,    1},  { 32,  0,  5,    2},
     { 32,  0,  5,    3},  { 32,  0,  5,    5},
     { 32,  0,  5,    6},  { 32,  0,  5,    8},
     { 32,  0,  5,    9},  { 32,  0,  5,   11},
     { 32,  0,  5,   12},  {  0,  0,  6,   15},
     { 32,  1,  5,   18},  { 32,  1,  5,   20},
     { 32,  2,  5,   24},  { 32,  2,  5,   28},
     { 32,  3,  5,   40},  { 32,  4,  5,   48},
     {  0, 16,  6,65536},  {  0, 15,  6,32768},
     {  0, 14,  6,16384},  {  0, 13,  6, 8192},
};   /* LL_defaultDTable */

/* Default FSE distribution table for Offset Codes */
static const __mram struct SeqSymbol OF_defaultDTable[(1<<OF_DEFAULTNORMLOG)+1] = {
    {  1,  1,  1, OF_DEFAULTNORMLOG},  /* header : fastMode, tableLog */
    /* nextState, nbAddBits, nbBits, baseVal */
    {  0,  0,  5,    0},     {  0,  6,  4,   61},
    {  0,  9,  5,  509},     {  0, 15,  5,32765},
    {  0, 21,  5,2097149},   {  0,  3,  5,    5},
    {  0,  7,  4,  125},     {  0, 12,  5, 4093},
    {  0, 18,  5,262141},    {  0, 23,  5,8388605},
    {  0,  5,  5,   29},     {  0,  8,  4,  253},
    {  0, 14,  5,16381},     {  0, 20,  5,1048573},
    {  0,  2,  5,    1},     { 16,  7,  4,  125},
    {  0, 11,  5, 2045},     {  0, 17,  5,131069},
    {  0, 22,  5,4194301},   {  0,  4,  5,   13},
    { 16,  8,  4,  253},     {  0, 13,  5, 8189},
    {  0, 19,  5,524285},    {  0,  1,  5,    1},
    { 16,  6,  4,   61},     {  0, 10,  5, 1021},
    {  0, 16,  5,65533},     {  0, 28,  5,268435453},
    {  0, 27,  5,134217725}, {  0, 26,  5,67108861},
    {  0, 25,  5,33554429},  {  0, 24,  5,16777213},
};   /* OF_defaultDTable */

/* Default FSE distribution table for Match Lengths */
static const __mram struct SeqSymbol ML_defaultDTable[(1<<ML_DEFAULTNORMLOG)+1] = {
    {  1,  1,  1, ML_DEFAULTNORMLOG},  /* header : fastMode, tableLog */
    /* nextState, nbAddBits, nbBits, baseVal */
    {  0,  0,  6,    3},  {  0,  0,  4,    4},
    { 32,  0,  5,    5},  {  0,  0,  5,    6},
    {  0,  0,  5,    8},  {  0,  0,  5,    9},
    {  0,  0,  5,   11},  {  0,  0,  6,   13},
    {  0,  0,  6,   16},  {  0,  0,  6,   19},
    {  0,  0,  6,   22},  {  0,  0,  6,   25},
    {  0,  0,  6,   28},  {  0,  0,  6,   31},
    {  0,  0,  6,   34},  {  0,  1,  6,   37},
    {  0,  1,  6,   41},  {  0,  2,  6,   47},
    {  0,  3,  6,   59},  {  0,  4,  6,   83},
    {  0,  7,  6,  131},  {  0,  9,  6,  515},
    { 16,  0,  4,    4},  {  0,  0,  4,    5},
    { 32,  0,  5,    6},  {  0,  0,  5,    7},
    { 32,  0,  5,    9},  {  0,  0,  5,   10},
    {  0,  0,  6,   12},  {  0,  0,  6,   15},
    {  0,  0,  6,   18},  {  0,  0,  6,   21},
    {  0,  0,  6,   24},  {  0,  0,  6,   27},
    {  0,  0,  6,   30},  {  0,  0,  6,   33},
    {  0,  1,  6,   35},  {  0,  1,  6,   39},
    {  0,  2,  6,   43},  {  0,  3,  6,   51},
    {  0,  4,  6,   67},  {  0,  5,  6,   99},
    {  0,  8,  6,  259},  { 32,  0,  4,    4},
    { 48,  0,  4,    4},  { 16,  0,  4,    5},
    { 32,  0,  5,    7},  { 32,  0,  5,    8},
    { 32,  0,  5,   10},  { 32,  0,  5,   11},
    {  0,  0,  6,   14},  {  0,  0,  6,   17},
    {  0,  0,  6,   20},  {  0,  0,  6,   23},
    {  0,  0,  6,   26},  {  0,  0,  6,   29},
    {  0,  0,  6,   32},  {  0, 16,  6,65539},
    {  0, 15,  6,32771},  {  0, 14,  6,16387},
    {  0, 13,  6, 8195},  {  0, 12,  6, 4099},
    {  0, 11,  6, 2051},  {  0, 10,  6, 1027},
};   /* ML_defaultDTable */

#define WILDCOPY_OVERLENGTH 32
#define WILDCOPY_VECLEN 16

#define SEQSYMBOL_TABLE_SIZE(log)   (1 + (1 << (log)))
#define HUF_DTABLE_SIZE(maxTableLog)   (1 + (1<<(maxTableLog)))

#define HufLog 12

static __mram_noinit u8 litStorage[NR_TASKLETS][ZSTD_BLOCKSIZE_MAX + WILDCOPY_OVERLENGTH];
static __mram_noinit struct SeqSymbol LLTableStorage[NR_TASKLETS][SEQSYMBOL_TABLE_SIZE(LLFSELog)];
static __mram_noinit struct SeqSymbol OFTableStorage[NR_TASKLETS][SEQSYMBOL_TABLE_SIZE(OffFSELog)];
static __mram_noinit struct SeqSymbol MLTableStorage[NR_TASKLETS][SEQSYMBOL_TABLE_SIZE(MLFSELog)];
static __mram_noinit u32 hufTableStorage[NR_TASKLETS][HUF_DTABLE_SIZE(HufLog)];

typedef enum {
    ZSTD_no_overlap,
    ZSTD_overlap_src_before_dst
} ZSTD_overlap;

#define HUF_TABLELOG_MAX      12      /* max runtime value of tableLog (due to static allocation); can be modified up to HUF_ABSOLUTEMAX_TABLELOG */
#define HUF_TABLELOG_DEFAULT  11      /* default tableLog value when none specified */
#define HUF_SYMBOLVALUE_MAX  255

#define HUF_TABLELOG_ABSOLUTEMAX  15  /* absolute limit of HUF_MAX_TABLELOG. Beyond that value, code does not work */
#if (HUF_TABLELOG_MAX > HUF_TABLELOG_ABSOLUTEMAX)
#  error "HUF_TABLELOG_MAX is too large !"
#endif

#define HUF_DECOMPRESS_WORKSPACE_SIZE (2 << 10)
#define HUF_DECOMPRESS_WORKSPACE_SIZE_U32 (HUF_DECOMPRESS_WORKSPACE_SIZE / sizeof(u32))

#define FSE_MAX_MEMORY_USAGE 14

#define FSE_MAX_TABLELOG  (FSE_MAX_MEMORY_USAGE-2)
#define FSE_MIN_TABLELOG 5
#define FSE_TABLELOG_ABSOLUTE_MAX 15
#if FSE_MAX_TABLELOG > FSE_TABLELOG_ABSOLUTE_MAX
#  error "FSE_MAX_TABLELOG > FSE_TABLELOG_ABSOLUTE_MAX is not supported"
#endif

#define FSE_TABLESTEP(tableSize) ((tableSize>>1) + (tableSize>>3) + 3)

#define FSE_MAX_SYMBOL_VALUE 255

#define FSE_DTABLE_SIZE_U32(maxTableLog)                   (1 + (1<<maxTableLog))

struct MramStream {
    seqreader_t reader;
    u8 *ptr;
};

typedef enum { bt_raw, bt_rle, bt_compressed, bt_reserved } BlockType;

typedef enum { set_basic, set_rle, set_compressed, set_repeat } SymbolEncodingType;

struct FrameHeader {
    size_t windowSize;
    size_t frameContentSize;
    u32 dictID;
    bool checksumFlag;
};

struct EntropyTables {
    __mram_ptr struct SeqSymbol *LLTable;
    __mram_ptr struct SeqSymbol *OFTable;
    __mram_ptr struct SeqSymbol *MLTable;
    __mram_ptr u32 *hufTable;
    u32 rep[ZSTD_REP_NUM];
};

struct FrameContext {
    struct FrameHeader *header;
    struct EntropyTables entropy;

    u32 workspace[HUF_DECOMPRESS_WORKSPACE_SIZE_U32];

    const __mram_ptr void* prefixStart;
    const __mram_ptr void* virtualStart;
    const __mram_ptr void* dictEnd;

    const __mram_ptr struct SeqSymbol *LLTptr;
    const __mram_ptr struct SeqSymbol *MLTptr;
    const __mram_ptr struct SeqSymbol *OFTptr;
    const __mram_ptr u8 *litPtr;
    const __mram_ptr u32 *hufPtr;
    size_t litSize;

    __mram_ptr u8 *litBuffer;
};

typedef struct { u16 v; } __attribute__((packed)) unalign16;
typedef struct { u32 v; } __attribute__((packed)) unalign32;
typedef struct { u64 v; } __attribute__((packed)) unalign64;

static u32 BIT_highbit32(u32 val) {
    return __builtin_clz(val) ^ 31;
}

unsigned MEM_32bits(void) { return sizeof(size_t)==4; }
unsigned MEM_64bits(void) { return sizeof(size_t)==8; }

u16 WRAM_read16(const void *ptr) { return ((const unalign16*)ptr)->v; }
u32 WRAM_read32(const void *ptr) { return ((const unalign32*)ptr)->v; }
u64 WRAM_read64(const void *ptr) { return ((const unalign64*)ptr)->v; }

u32 WRAM_read24(const void *ptr) {
    return WRAM_read16(ptr) + (((const u8*)ptr)[2] << 16);
}

#define DMA_ALIGNMENT 8
#define DMA_OFF_MASK (DMA_ALIGNMENT - 1)
#define DMA_ALIGNED(x) ALIGN(x, DMA_ALIGNMENT)

u32 MRAM_read32(const __mram_ptr void *ptr) {
    u32 off = ((uintptr_t)ptr) & DMA_OFF_MASK;
    if (off <= 4) {
        u64 cache;
        mram_read(ptr, &cache, 8);
        switch (off) {
            case 0:
                return (u32) cache;
            case 4:
                return (u32) (cache >> 32);
            default:
                return (u32) (cache >> (off * 8));
        }
    } else {
        u64 cache[2];
        mram_read(ptr, cache, 16);
        return (((u32)(cache[0] >> 32)) >> ((off - 4) * 8)) | (((u32)cache[1]) << ((8 - off) * 8));
    }
}

u64 MRAM_read64(const __mram_ptr void *ptr) { return ((const __mram_ptr unalign64*)ptr)->v; }

size_t WRAM_readST(const void *ptr) {
    if (MEM_32bits()) {
        return (size_t)WRAM_read32(ptr);
    } else {
        return (size_t)WRAM_read64(ptr);
    }
}

#define MRAM_CACHE_SIZE 32

static __dma_aligned u8 mramReadCache[NR_TASKLETS][MRAM_CACHE_SIZE];
static __dma_aligned u8 mramWriteCache[NR_TASKLETS][MRAM_CACHE_SIZE];

#define DEFINE_MRAM_copyN(size) \
static inline void MRAM_copy##size(__mram_ptr void *dst, const __mram_ptr void *src) { \
    u8 *srcCache = mramReadCache[me()]; \
    u8 *dstCache = mramWriteCache[me()]; \
    u32 srcOff = ((uintptr_t)src) & DMA_OFF_MASK; \
    u32 dstOff = ((uintptr_t)dst) & DMA_OFF_MASK; \
    u32 sizeDma = DMA_ALIGNED(size); \
    if (dstOff == 0) { \
        if (srcOff == 0) { \
            mram_read(src, dstCache, sizeDma); \
        } else { \
            mram_read(src, srcCache, sizeDma + 8); \
            memcpy(dstCache, srcCache + srcOff, size); \
        } \
        mram_write(dstCache, dst, sizeDma); \
    } else { \
        mram_read(dst, dstCache, sizeDma + 8); \
        if (srcOff == 0) { \
            mram_read(src, srcCache, sizeDma); \
            memcpy(dstCache + dstOff, srcCache, size); \
        } else { \
            mram_read(src, srcCache, sizeDma + 8); \
            memcpy(dstCache + dstOff, srcCache + srcOff, size); \
        } \
        mram_write(dstCache, dst, sizeDma + 8); \
    } \
}

DEFINE_MRAM_copyN(4)
DEFINE_MRAM_copyN(8)
DEFINE_MRAM_copyN(16)

static void MRAM_memcpy(__mram_ptr void *dst, const __mram_ptr void *src, size_t length) {
    u8 *srcCache = mramReadCache[me()];
    u8 *dstCache = mramWriteCache[me()];

    u32 srcOff = ((uintptr_t)src) & DMA_OFF_MASK;
    u32 dstOff = ((uintptr_t)dst) & DMA_OFF_MASK;
    size_t remaining = length;

    if (dstOff != 0) {
        mram_read(dst, dstCache, DMA_ALIGNED(dstOff));
    }

    if (srcOff == dstOff) {
        u32 idx = 0;
        size_t len = MIN(remaining, MRAM_CACHE_SIZE - srcOff);

        mram_read(src, srcCache, MRAM_CACHE_SIZE);
        memcpy(dstCache + dstOff, srcCache + srcOff, len);
        mram_write(dstCache, dst, MRAM_CACHE_SIZE);
        remaining -= len;
        idx += len;

        while (remaining >= MRAM_CACHE_SIZE) {
            mram_read(src + idx, srcCache, MRAM_CACHE_SIZE);
            mram_write(srcCache, dst + idx, MRAM_CACHE_SIZE);
            remaining -= MRAM_CACHE_SIZE;
            idx += MRAM_CACHE_SIZE;
        }

        if (remaining != 0) {
            mram_read(dst + idx, dstCache, MRAM_CACHE_SIZE);
            mram_read(src + idx, srcCache, MRAM_CACHE_SIZE);
            memcpy(dstCache, srcCache, remaining);
            mram_write(dstCache, dst + idx, MRAM_CACHE_SIZE);
        }
    } else {
        u32 srcIdx = 0;
        u32 dstIdx = 0;
        mram_read(src, srcCache, MRAM_CACHE_SIZE);

        srcIdx += MRAM_CACHE_SIZE;
        while (remaining != 0) {
            if (dstOff > srcOff) {
                size_t len = MIN(remaining, MRAM_CACHE_SIZE - dstOff);
                memcpy(dstCache + dstOff, srcCache + srcOff, len);
                mram_write(dstCache, dst + dstIdx, MRAM_CACHE_SIZE);
                remaining -= len;
                dstIdx += MRAM_CACHE_SIZE;
                srcOff += len;
                dstOff = 0;
            } else {
                size_t len = MIN(remaining, MRAM_CACHE_SIZE - srcOff);
                memcpy(dstCache + dstOff, srcCache + srcOff, len);
                mram_read(src + srcIdx, srcCache, MRAM_CACHE_SIZE);
                srcIdx += MRAM_CACHE_SIZE;
                dstOff += len;
                srcOff = 0;
            }
        }
    }
}

static void MRAM_memmove(__mram_ptr void *dst, const __mram_ptr void *src, size_t length) {
    // TODO
    (void) dst;
    (void) src;
    (void) length;
    abort();
}

static void MRAM_memset(__mram_ptr void *dst, u8 value, size_t length) {
    u8 *cache = mramWriteCache[me()];

    u32 offset = ((uintptr_t)dst) & (MRAM_CACHE_SIZE - 1);
    u32 idx = 0;
    size_t remaining = length;

    if (offset != 0) {
        mram_read(dst, cache, MRAM_CACHE_SIZE);
        size_t part = MIN(MRAM_CACHE_SIZE - offset, length);
        memset(cache + offset, value, part);
        mram_write(cache, dst, MRAM_CACHE_SIZE);
        remaining -= part;
        idx += part;
    }

    if (remaining >= MRAM_CACHE_SIZE) {
        memset(cache, value, (offset == 0) ? MRAM_CACHE_SIZE : offset);
        do {
            mram_write(cache, dst + idx, MRAM_CACHE_SIZE);
            idx += MRAM_CACHE_SIZE;
            remaining -= MRAM_CACHE_SIZE;
        } while (remaining >= MRAM_CACHE_SIZE);
    }

    if (remaining > 0) {
        mram_read(dst + idx, cache, MRAM_CACHE_SIZE);
        memset(cache, value, remaining);
        mram_write(cache, dst + idx, MRAM_CACHE_SIZE);
    }
}

static void streamInit(struct MramStream *stream, __mram_ptr const void *src) {
    stream->ptr = seqread_init(seqread_alloc(), (__mram_ptr void *)src, &stream->reader);
}

static u8 *streamSetAt(struct MramStream *stream, __mram_ptr const void *src) {
    return stream->ptr = seqread_seek((__mram_ptr void *)src, &stream->reader);
}

static u8 *streamAdvance(struct MramStream *stream, size_t increment) {
    return stream->ptr = seqread_get(stream->ptr, increment, &stream->reader);
}

static uintptr_t streamGetMramAddr(struct MramStream *stream) {
    return (uintptr_t)seqread_tell(stream->ptr, &stream->reader);
}

static size_t getFrameHeader(struct FrameHeader *frameHeader, const void *src) {
    const u8* ip = src;
    size_t const minInputSize = 5;
    u32 const magicNumber = WRAM_read32(ip);

#if USE_DEF_GUARDS
    if (unlikely(magicNumber != ZSTD_MAGIC_NUMBER)) {
        abort();
    }
#else
    (void) magicNumber;
#endif

    u8 fhdByte = ip[minInputSize - 1];
    size_t pos = minInputSize;
    u32 const dictIDSizeCode = fhdByte & 3;
    bool const checksumFlag = ((fhdByte >> 2) & 1) != 0;
    bool const singleSegment = ((fhdByte >> 5) & 1) != 0;
    u32 const fcsID = fhdByte >> 6;
    u32 windowSize = 0;
    u32 dictID = 0;
    u32 frameContentSize = ZSTD_CONTENTSIZE_UNKNOWN;

    if (!singleSegment) {
        u8 const wlByte = ip[pos++];
        u32 const windowLog = (wlByte >> 3) + ZSTD_WINDOWLOG_ABSOLUTEMIN;
        windowSize = (1U << windowLog);
        windowSize += (windowSize >> 3) * (wlByte & 7);
    }

    switch(dictIDSizeCode) {
        default: /* Impossible */
        case 0: break;
        case 1: dictID = ip[pos]; pos++; break;
        case 2: dictID = WRAM_read16(ip+pos); pos+=2; break;
        case 3: dictID = WRAM_read32(ip+pos); pos+=4; break;
    }

    switch(fcsID) {
        default: /* Impossible */
        case 0 : if (singleSegment) frameContentSize = ip[pos]; pos++; break;
        case 1 : frameContentSize = WRAM_read16(ip+pos)+256; pos+=2; break;
        case 2 : frameContentSize = WRAM_read32(ip+pos); pos+=4; break;
        case 3 : frameContentSize = WRAM_read64(ip+pos); pos+=8; break;
    }

    if (singleSegment) windowSize = frameContentSize;

    frameHeader->frameContentSize = frameContentSize;
    frameHeader->windowSize = windowSize;
    frameHeader->dictID = dictID;
    frameHeader->checksumFlag = checksumFlag;

    return pos;
}

static size_t copyRawBlock(__mram_ptr void *dst, size_t dstCapacity, struct MramStream *src, size_t srcSize) {
 #if USE_DEF_GUARDS
    if (unlikely(srcSize > dstCapacity)) {
        abort();
    }
#else
    (void) dstCapacity;
#endif
    __mram_ptr void * start = (__mram_ptr void*) streamGetMramAddr(src);
    MRAM_memcpy(dst, start, srcSize);
    streamSetAt(src, start + srcSize);
    return srcSize;
}

static size_t setRleBlock(__mram_ptr void *dst, size_t dstCapacity, u8 value, size_t regenSize) {
#if USE_DEF_GUARDS
    if (unlikely(regenSize > dstCapacity)) {
        abort();
    }
#else
    (void) dstCapacity;
#endif
    MRAM_memset(dst, value, regenSize);
    return regenSize;
}

struct HUF_DEltX1 {
    u8 byte;
    u8 nbBits;
};

struct DTableDesc { u8 maxTableLog; u8 tableType; u8 tableLog; u8 reserved; };

static struct DTableDesc HUF_getDTableDesc(const __mram_ptr u32* table) {
    struct DTableDesc dtd;
    u32 value = *table;
    dtd.maxTableLog = value & 0xff;
    dtd.tableType = (value >> 8) & 0xff;
    dtd.tableLog = (value >> 16) & 0xff;
    return dtd;
}

static size_t FSE_readNCount(short *normalizedCounter, unsigned *maxSVPtr, unsigned *tableLogPtr, struct MramStream *headerBuffer, size_t hbSize) {
    uintptr_t const istart = streamGetMramAddr(headerBuffer);
    uintptr_t const iend = istart + hbSize;
    uintptr_t ipMram = istart;
    const u8 *ip = headerBuffer->ptr;
    int nbBits;
    int remaining;
    int threshold;
    u32 bitStream;
    int bitCount;
    unsigned charnum = 0;
    int previous0 = 0;

    if (hbSize < 4) {
        // TODO
        abort();
    }

    /* init */
    memset(normalizedCounter, 0, (*maxSVPtr+1) * sizeof(normalizedCounter[0]));   /* all symbols not present in NCount have a frequency of 0 */
    bitStream = WRAM_read32(ip);
    nbBits = (bitStream & 0xF) + FSE_MIN_TABLELOG;   /* extract tableLog */
#if USE_DEF_GUARDS
    if (unlikely(nbBits > FSE_TABLELOG_ABSOLUTE_MAX)) {
        abort();
    }
#endif
    bitStream >>= 4;
    bitCount = 4;
    *tableLogPtr = nbBits;
    remaining = (1<<nbBits)+1;
    threshold = 1<<nbBits;
    nbBits++;

    while ((remaining>1) & (charnum<=*maxSVPtr)) {
		if (previous0) {
			unsigned n0 = charnum;
			while ((bitStream & 0xFFFF) == 0xFFFF) {
				n0 += 24;
                if (ipMram < iend-5) {
                    ipMram += 2;
                    ip = streamAdvance(headerBuffer, 2);
                    bitStream = WRAM_read32(ip) >> bitCount;
                } else {
                    bitStream >>= 16;
                    bitCount   += 16;
                }
			}
            while ((bitStream & 3) == 3) {
                n0 += 3;
                bitStream >>= 2;
                bitCount += 2;
		    }
            n0 += bitStream & 3;
            bitCount += 2;
#if USE_DEF_GUARDS
            if (unlikely(n0 > *maxSVPtr)) {
                abort();
            }
#endif
            while (charnum < n0) normalizedCounter[charnum++] = 0;
            if ((ipMram <= iend-7) || (ipMram + (bitCount>>3) <= iend-4)) {
#if USE_DEF_GUARDS
                if (unlikely((bitCount >> 3) > 3)) {
                    abort();
                }
#endif
                ipMram += bitCount>>3;
                ip = streamAdvance(headerBuffer, bitCount>>3);
                bitCount &= 7;
                bitStream = WRAM_read32(ip) >> bitCount;
            } else {
                bitStream >>= 2;
            }
        }
        {
            int const max = (2*threshold-1) - remaining;
            int count;

            if ((bitStream & (threshold-1)) < (u32)max) {
                count = bitStream & (threshold-1);
                bitCount += nbBits-1;
            } else {
                count = bitStream & (2*threshold-1);
                if (count >= threshold) count -= max;
                bitCount += nbBits;
            }

            count--;   /* extra accuracy */
            remaining -= count < 0 ? -count : count;   /* -1 means +1 */
            normalizedCounter[charnum++] = (short)count;
            previous0 = !count;
            while (remaining < threshold) {
                nbBits--;
                threshold >>= 1;
            }

            if ((ipMram <= iend-7) || (ipMram + (bitCount>>3) <= iend-4)) {
                ipMram += bitCount>>3;
                ip = streamAdvance(headerBuffer, bitCount>>3);
                bitCount &= 7;
            } else {
                bitCount -= (int)(8 * (iend - 4 - ipMram));
                ipMram = iend - 4;
                ip = streamSetAt(headerBuffer, (__mram_ptr void const *)ipMram);
            }
            bitStream = WRAM_read32(ip) >> (bitCount & 31);
        }
    }
#if USE_DEF_GUARDS
    if (unlikely(remaining != 1)) {
        abort();
    }
    if (unlikely(bitCount > 32)) {
        abort();
    }
#endif
    *maxSVPtr = charnum-1;

    ipMram += (bitCount+7)>>3;
    streamAdvance(headerBuffer, (bitCount+7)>>3);
    return ipMram-istart;
}

struct FSE_DTableHeader {
    u16 tableLog;
    u16 fastMode;
};

struct FSE_decode {
    unsigned short newState;
    unsigned char  symbol;
    unsigned char  nbBits;
};   /* size == U32 */

static size_t FSE_buildDTable(u32 *dt, const short* normalizedCounter, unsigned maxSymbolValue, unsigned tableLog) {
    void* const tdPtr = dt+1;   /* because *dt is unsigned, 32-bits aligned on 32-bits */
    struct FSE_decode *const tableDecode = (struct FSE_decode*) (tdPtr);
    u16 symbolNext[FSE_MAX_SYMBOL_VALUE+1];

    u32 const maxSV1 = maxSymbolValue + 1;
    u32 const tableSize = 1 << tableLog;
    u32 highThreshold = tableSize-1;

#if USE_DEF_GUARDS
    /* Sanity Checks */
    if (unlikely(maxSymbolValue > FSE_MAX_SYMBOL_VALUE)) {
        abort();
    }
    if (unlikely(tableLog > FSE_MAX_TABLELOG)) {
        abort();
    }
#endif

    /* Init, lay down lowprob symbols */
    {
        struct FSE_DTableHeader DTableH;
        DTableH.tableLog = (u16)tableLog;
        DTableH.fastMode = 1;
        {   s16 const largeLimit= (s16)(1 << (tableLog-1));
            u32 s;
            for (s=0; s<maxSV1; s++) {
                if (normalizedCounter[s]==-1) {
                    tableDecode[highThreshold--].symbol = (u8)s;
                    symbolNext[s] = 1;
                } else {
                    if (normalizedCounter[s] >= largeLimit) DTableH.fastMode=0;
                    symbolNext[s] = normalizedCounter[s];
        }   }   }
        memcpy(dt, &DTableH, sizeof(DTableH));
    }

    /* Spread symbols */
    {
        u32 const tableMask = tableSize-1;
        u32 const step = FSE_TABLESTEP(tableSize);
        u32 s, position = 0;
        for (s=0; s<maxSV1; s++) {
            int i;
            for (i=0; i<normalizedCounter[s]; i++) {
                tableDecode[position].symbol = (u8)s;
                position = (position + step) & tableMask;
                while (position > highThreshold) position = (position + step) & tableMask;   /* lowprob area */
            }
        }
#if USE_DEF_GUARDS
        /* position must reach all cells once, otherwise normalizedCounter is incorrect */
        if (unlikely(position != 0)) {
            abort();
        }
#endif
    }

    /* Build Decoding table */
    {
        u32 u;
        for (u=0; u<tableSize; u++) {
            u8 const symbol = (u8)(tableDecode[u].symbol);
            u32 const nextState = symbolNext[symbol]++;
            tableDecode[u].nbBits = (u8) (tableLog - BIT_highbit32(nextState) );
            tableDecode[u].newState = (u16) ( (nextState << tableDecode[u].nbBits) - tableSize);
        }
    }

    return 0;
}

struct FSE_DState {
    size_t      state;
    const void *table;
};

static __dma_aligned char bitDStreamBuffers[NR_TASKLETS][4][MRAM_CACHE_SIZE];

struct BIT_DStream {
    size_t   bitContainer;
    unsigned bitsConsumed;
    const __mram_ptr char* ptr;
    const __mram_ptr char* start;
    const __mram_ptr char* limitPtr;

    char *cache;
    const __mram_ptr char* cacheStart;
};

static size_t BIT_readST(struct BIT_DStream *bitD) {
    s32 idx = bitD->ptr - bitD->cacheStart;

    if (unlikely(idx < 0)) {
        bitD->cacheStart = (const __mram_ptr char*)DMA_ALIGNED((uintptr_t)bitD->ptr - MRAM_CACHE_SIZE + sizeof(bitD->bitContainer));
        mram_read(bitD->cacheStart, bitD->cache, MRAM_CACHE_SIZE);
        idx = bitD->ptr - bitD->cacheStart;
    }

    return WRAM_readST(((char*)bitD->cache) + idx);
}

typedef enum { BIT_DStream_unfinished = 0,
               BIT_DStream_endOfBuffer = 1,
               BIT_DStream_completed = 2,
               BIT_DStream_overflow = 3 } BIT_DStream_status;  /* result of BIT_reloadDStream() */
               /* 1,2,4,8 would be better for bitmap combinations, but slows down performance a bit ... :( */

static const unsigned BIT_mask[] = {
    0,          1,         3,         7,         0xF,       0x1F,
    0x3F,       0x7F,      0xFF,      0x1FF,     0x3FF,     0x7FF,
    0xFFF,      0x1FFF,    0x3FFF,    0x7FFF,    0xFFFF,    0x1FFFF,
    0x3FFFF,    0x7FFFF,   0xFFFFF,   0x1FFFFF,  0x3FFFFF,  0x7FFFFF,
    0xFFFFFF,   0x1FFFFFF, 0x3FFFFFF, 0x7FFFFFF, 0xFFFFFFF, 0x1FFFFFFF,
    0x3FFFFFFF, 0x7FFFFFFF}; /* up to 31 bits */
#define BIT_MASK_SIZE (sizeof(BIT_mask) / sizeof(BIT_mask[0]))

static size_t BIT_initDStream(struct BIT_DStream *bitD, const __mram_ptr void *srcBuffer, size_t srcSize, char *cache) {
#if USE_DEF_GUARDS
    if (unlikely(srcSize < 1)) {
        abort();
    }
#endif

    bitD->cache = cache;
    bitD->start = (__mram_ptr char*)srcBuffer;
    bitD->limitPtr = bitD->start + sizeof(bitD->bitContainer);

    if (srcSize >= sizeof(bitD->bitContainer)) {
        /* normal case */
        bitD->ptr = bitD->start + srcSize - sizeof(bitD->bitContainer);
        bitD->cacheStart = bitD->ptr + 1;  /* dummy value to ensure a MRAM cache reload */
        bitD->bitContainer = BIT_readST(bitD);
        {
            u8 const lastByte = bitD->start[srcSize-1];
            bitD->bitsConsumed = lastByte ? 8 - BIT_highbit32(lastByte) : 0;  /* ensures bitsConsumed is always set */
#if USE_DEF_GUARDS
            if (unlikely(lastByte == 0)) {
                /* endMark not present */
                abort();
            }
#endif
        }
    } else {
        // TODO
        abort();
    }

    return srcSize;
}

static __attribute__((unused)) size_t BIT_getUpperBits(size_t bitContainer, u32 const start)
{
    return bitContainer >> start;
}

static size_t BIT_getMiddleBits(size_t bitContainer, u32 const start, u32 const nbBits)
{
    u32 const regMask = sizeof(bitContainer)*8 - 1;
    /* if start > regMask, bitstream is corrupted, and result is undefined */
#if USE_DEF_GUARDS
    if (unlikely(nbBits >= BIT_MASK_SIZE)) {
        abort();
    }
#endif
    return (bitContainer >> (start & regMask)) & BIT_mask[nbBits];
}

static __attribute__((unused)) size_t BIT_getLowerBits(size_t bitContainer, u32 const nbBits)
{
#if USE_DEF_GUARDS
    if (unlikely(nbBits >= BIT_MASK_SIZE)) {
        abort();
    }
#endif
    return bitContainer & BIT_mask[nbBits];
}

static size_t BIT_lookBits(const struct BIT_DStream* bitD, u32 nbBits)
{
    /* if bitD->bitsConsumed + nbBits > sizeof(bitD->bitContainer)*8,
     * bitstream is likely corrupted, and result is undefined */
    return BIT_getMiddleBits(bitD->bitContainer, (sizeof(bitD->bitContainer)*8) - bitD->bitsConsumed - nbBits, nbBits);
}

static size_t BIT_lookBitsFast(const struct BIT_DStream* bitD, u32 nbBits)
{
    u32 const regMask = sizeof(bitD->bitContainer)*8 - 1;
#if USE_DEF_GUARDS
    if (unlikely(nbBits < 1)) {
        abort();
    }
#endif
    return (bitD->bitContainer << (bitD->bitsConsumed & regMask)) >> (((regMask+1)-nbBits) & regMask);
}

static void BIT_skipBits(struct BIT_DStream* bitD, u32 nbBits)
{
    bitD->bitsConsumed += nbBits;
}

static size_t BIT_readBits(struct BIT_DStream* bitD, unsigned nbBits)
{
    size_t const value = BIT_lookBits(bitD, nbBits);
    BIT_skipBits(bitD, nbBits);
    return value;
}

static size_t BIT_readBitsFast(struct BIT_DStream* bitD, unsigned nbBits)
{
    size_t const value = BIT_lookBitsFast(bitD, nbBits);
#if USE_DEF_GUARDS
    if (unlikely(nbBits < 1)) {
        abort();
    }
#endif
    BIT_skipBits(bitD, nbBits);
    return value;
}

static BIT_DStream_status BIT_reloadDStreamFast(struct BIT_DStream* bitD) {
    if (unlikely(bitD->ptr < bitD->limitPtr)) {
        return BIT_DStream_overflow;
    }
#if USE_DEF_GUARDS
    if (unlikely(bitD->bitsConsumed > sizeof(bitD->bitContainer)*8)) {
        abort();
    }
#endif
    bitD->ptr -= bitD->bitsConsumed >> 3;
    bitD->bitsConsumed &= 7;
    bitD->bitContainer = BIT_readST(bitD);
    return BIT_DStream_unfinished;
}

BIT_DStream_status BIT_reloadDStream(struct BIT_DStream* bitD) {
    if (bitD->bitsConsumed > (sizeof(bitD->bitContainer)*8))  /* overflow detected, like end of stream */
        return BIT_DStream_overflow;

    if (bitD->ptr >= bitD->limitPtr) {
        return BIT_reloadDStreamFast(bitD);
    }
    if (bitD->ptr == bitD->start) {
        if (bitD->bitsConsumed < sizeof(bitD->bitContainer)*8) return BIT_DStream_endOfBuffer;
        return BIT_DStream_completed;
    }
    /* start < ptr < limitPtr */
    {   u32 nbBytes = bitD->bitsConsumed >> 3;
        BIT_DStream_status result = BIT_DStream_unfinished;
        if (bitD->ptr - nbBytes < bitD->start) {
            nbBytes = (u32)(bitD->ptr - bitD->start);  /* ptr > start */
            result = BIT_DStream_endOfBuffer;
        }
        bitD->ptr -= nbBytes;
        bitD->bitsConsumed -= nbBytes*8;
        bitD->bitContainer = BIT_readST(bitD);   /* reminder : srcSize > sizeof(bitD->bitContainer), otherwise bitD->ptr == bitD->start */
        return result;
    }
}

static __attribute__((unused)) unsigned BIT_endOfDStream(const struct BIT_DStream* DStream)
{
    return ((DStream->ptr == DStream->start) && (DStream->bitsConsumed == sizeof(DStream->bitContainer)*8));
}

static __attribute__((unused)) u8 FSE_peekSymbol(const struct FSE_DState* DStatePtr)
{
    struct FSE_decode const DInfo = ((const struct FSE_decode*)(DStatePtr->table))[DStatePtr->state];
    return DInfo.symbol;
}

static __attribute__((unused)) void FSE_updateState(struct FSE_DState* DStatePtr, struct BIT_DStream* bitD)
{
    struct FSE_decode const DInfo = ((const struct FSE_decode*)(DStatePtr->table))[DStatePtr->state];
    u32 const nbBits = DInfo.nbBits;
    size_t const lowBits = BIT_readBits(bitD, nbBits);
    DStatePtr->state = DInfo.newState + lowBits;
}

static u8 FSE_decodeSymbol(struct FSE_DState* DStatePtr, struct BIT_DStream* bitD)
{
    struct FSE_decode const DInfo = ((const struct FSE_decode*)(DStatePtr->table))[DStatePtr->state];
    u32 const nbBits = DInfo.nbBits;
    u8 const symbol = DInfo.symbol;
    size_t const lowBits = BIT_readBits(bitD, nbBits);

    DStatePtr->state = DInfo.newState + lowBits;
    return symbol;
}

static u8 FSE_decodeSymbolFast(struct FSE_DState* DStatePtr, struct BIT_DStream* bitD)
{
    struct FSE_decode const DInfo = ((const struct FSE_decode*)(DStatePtr->table))[DStatePtr->state];
    u32 const nbBits = DInfo.nbBits;
    u8 const symbol = DInfo.symbol;
    size_t const lowBits = BIT_readBitsFast(bitD, nbBits);

    DStatePtr->state = DInfo.newState + lowBits;
    return symbol;
}

static __attribute__((unused)) unsigned FSE_endOfDState(const struct FSE_DState* DStatePtr)
{
    return DStatePtr->state == 0;
}


void FSE_initDState(struct FSE_DState* DStatePtr, struct BIT_DStream* bitD, const u32* dt) {
    const void* ptr = dt;
    const struct FSE_DTableHeader* const DTableH = (const struct FSE_DTableHeader*)ptr;
    DStatePtr->state = BIT_readBits(bitD, DTableH->tableLog);
    BIT_reloadDStream(bitD);
    DStatePtr->table = dt + 1;
}

static size_t FSE_decompress_usingDTable_generic(
          void* dst, size_t maxDstSize,
    struct MramStream *cSrc, size_t cSrcSize,
    const u32* dt, const unsigned fast) {
    u8* const ostart = (u8*) dst;
    u8* op = ostart;
    u8* const omax = op + maxDstSize;
    u8* const olimit = omax-3;

    struct BIT_DStream bitD;
    struct FSE_DState state1;
    struct FSE_DState state2;

    /* Init */
    __mram_ptr void* start = (__mram_ptr void*)streamGetMramAddr(cSrc);
    BIT_initDStream(&bitD, start, cSrcSize, bitDStreamBuffers[me()][0]);
    streamSetAt(cSrc, start + cSrcSize);

    FSE_initDState(&state1, &bitD, dt);
    FSE_initDState(&state2, &bitD, dt);

#define FSE_GETSYMBOL(statePtr) fast ? FSE_decodeSymbolFast(statePtr, &bitD) : FSE_decodeSymbol(statePtr, &bitD)

    /* 4 symbols per loop */
    for ( ; (BIT_reloadDStream(&bitD)==BIT_DStream_unfinished) & (op<olimit) ; op+=4) {
        op[0] = FSE_GETSYMBOL(&state1);

        if (FSE_MAX_TABLELOG*2+7 > sizeof(bitD.bitContainer)*8)    /* This test must be static */
            BIT_reloadDStream(&bitD);

        op[1] = FSE_GETSYMBOL(&state2);

        if (FSE_MAX_TABLELOG*4+7 > sizeof(bitD.bitContainer)*8)    /* This test must be static */
            { if (BIT_reloadDStream(&bitD) > BIT_DStream_unfinished) { op+=2; break; } }

        op[2] = FSE_GETSYMBOL(&state1);

        if (FSE_MAX_TABLELOG*2+7 > sizeof(bitD.bitContainer)*8)    /* This test must be static */
            BIT_reloadDStream(&bitD);

        op[3] = FSE_GETSYMBOL(&state2);
    }

    /* tail */
    /* note : BIT_reloadDStream(&bitD) >= FSE_DStream_partiallyFilled; Ends at exactly BIT_DStream_completed */
    while (1) {
#if USE_DEF_GUARDS
        if (unlikely(op>(omax-2))) {
            abort();
        }
#endif
        *op++ = FSE_GETSYMBOL(&state1);
        if (BIT_reloadDStream(&bitD)==BIT_DStream_overflow) {
            *op++ = FSE_GETSYMBOL(&state2);
            break;
        }

#if USE_DEF_GUARDS
        if (unlikely(op>(omax-2))) {
            abort();
        }
#endif
        *op++ = FSE_GETSYMBOL(&state2);
        if (BIT_reloadDStream(&bitD)==BIT_DStream_overflow) {
            *op++ = FSE_GETSYMBOL(&state1);
            break;
    }   }

    return op-ostart;
}


static size_t FSE_decompress_usingDTable(void* dst, size_t originalSize, struct MramStream *cSrc, size_t cSrcSize, const u32* dt) {
    const void* ptr = dt;
    const struct FSE_DTableHeader* DTableH = (const struct FSE_DTableHeader*)ptr;
    const u32 fastMode = DTableH->fastMode;

    /* select fast mode (static) */
    if (fastMode) return FSE_decompress_usingDTable_generic(dst, originalSize, cSrc, cSrcSize, dt, 1);
    return FSE_decompress_usingDTable_generic(dst, originalSize, cSrc, cSrcSize, dt, 0);
}

static size_t FSE_decompress_wksp(void* dst, size_t dstCapacity, struct MramStream *cSrc, size_t cSrcSize, u32* workSpace, u32 maxLog) {
    short counting[FSE_MAX_SYMBOL_VALUE+1];
    unsigned tableLog;
    unsigned maxSymbolValue = FSE_MAX_SYMBOL_VALUE;

    /* normal FSE decoding mode */
    size_t const NCountLength = FSE_readNCount(counting, &maxSymbolValue, &tableLog, cSrc, cSrcSize);
#if USE_DEF_GUARDS
    if (unlikely(tableLog > maxLog)) {
        abort();
    }
#else
    (void) maxLog;
#endif
    cSrcSize -= NCountLength;

    FSE_buildDTable(workSpace, counting, maxSymbolValue, tableLog);

    return FSE_decompress_usingDTable(dst, dstCapacity, cSrc, cSrcSize, workSpace);
}

static size_t HUF_readStats(u8 *huffWeight, size_t hwSize, u32 *rankStats, u32 *nbSymbolsPtr, u32 *tableLogPtr, struct MramStream *src, size_t srcSize) {
    u32 weightTotal;
    const u8 *ip = src->ptr;
    size_t iSize;
    size_t oSize;

#if USE_DEF_GUARDS
    if (unlikely(srcSize == 0)) {
        abort();
    }
#else
    (void) srcSize;
#endif

    iSize = ip[0];

    if (iSize >= 128) {
        /* special header */
        oSize = iSize - 127;
        iSize = ((oSize+1)/2);
#if USE_DEF_GUARDS
        if (unlikely(iSize+1 > srcSize)) {
            abort();
        }
        if (unlikely(oSize >= hwSize)) {
            abort();
        }
#endif
        ip += 1;
        for (u32 n = 0; n < oSize; n += 2) {
            huffWeight[n]   = ip[n/2] >> 4;
            huffWeight[n+1] = ip[n/2] & 15;
        }
        streamAdvance(src, 1 + iSize);
    } else {
        /* header compressed with FSE (normal case) */
        u32 fseWorkspace[FSE_DTABLE_SIZE_U32(6)]; /* 6 is max possible tableLog for HUF header (maybe even 5, to be tested) */
#if USE_DEF_GUARDS
        if (unlikely(iSize+1 > srcSize)) {
            abort();
        }
#endif
        streamAdvance(src, 1);
        oSize = FSE_decompress_wksp(huffWeight, hwSize-1, src, iSize, fseWorkspace, 6); /* max (hwSize-1) values decoded, as last one is implied */
    }

    /* collect weight stats */
    memset(rankStats, 0, (HUF_TABLELOG_MAX + 1) * sizeof(u32));
    weightTotal = 0;
    for (u32 n = 0; n < oSize; n++) {
#if USE_DEF_GUARDS
        if (unlikely(huffWeight[n] >= HUF_TABLELOG_MAX)) {
            abort();
        }
#endif
        rankStats[huffWeight[n]]++;
        weightTotal += (1 << huffWeight[n]) >> 1;
    }
#if USE_DEF_GUARDS
    if (unlikely(weightTotal == 0)) {
        abort();
    }
#endif

    /* get last non-null symbol weight (implied, total must be 2^n) */
    {
        u32 const tableLog = BIT_highbit32(weightTotal) + 1;
#if USE_DEF_GUARDS
        if (unlikely(tableLog > HUF_TABLELOG_MAX)) {
            abort();
        }
#endif
        *tableLogPtr = tableLog;
        /* determine last weight */
        {
            u32 const total = 1 << tableLog;
            u32 const rest = total - weightTotal;
            u32 const lastWeight = BIT_highbit32(rest) + 1;
#if USE_DEF_GUARDS
            u32 const verif = 1 << BIT_highbit32(rest);
            /* last value must be a clean power of 2 */
            if (unlikely(verif != rest)) {
                abort();
            }
#endif
            huffWeight[oSize] = (u8)lastWeight;
            rankStats[lastWeight]++;
        }
    }

#if USE_DEF_GUARDS
    if (unlikely((rankStats[1] < 2) || (rankStats[1] & 1))) {
        /* by construction : at least 2 elts of rank 1, must be even */
        abort();
    }
#endif

    /* results */
    *nbSymbolsPtr = (u32)(oSize+1);
    return iSize+1;
}

static size_t HUF_readTableX1(__mram_ptr u32 *DTable, struct MramStream *src, size_t srcSize, void *workspace, size_t wkspSize) {
    u32 tableLog = 0;
    u32 nbSymbols = 0;
    size_t iSize;
    __mram_ptr void *const dtPtr = DTable + 1;
    __mram_ptr struct HUF_DEltX1 *const dt = (__mram_ptr struct HUF_DEltX1 *)dtPtr;

    u32 *rankVal;
    u8 *huffWeight;
    size_t spaceUsed32 = 0;

    rankVal = (u32 *)workspace + spaceUsed32;
    spaceUsed32 += HUF_TABLELOG_ABSOLUTEMAX + 1;
    huffWeight = (u8 *)((u32 *)workspace + spaceUsed32);
    spaceUsed32 += ALIGN(HUF_SYMBOLVALUE_MAX + 1, sizeof(u32)) >> 2;

#if USE_DEF_GUARDS
    if (unlikely((spaceUsed32 << 2) > wkspSize)) {
        abort();
    }
#else
    (void) wkspSize;
#endif

    iSize = HUF_readStats(huffWeight, HUF_SYMBOLVALUE_MAX + 1, rankVal, &nbSymbols, &tableLog, src, srcSize);

    /* Table header */
    {
        struct DTableDesc dtd = HUF_getDTableDesc(DTable);
#if USE_DEF_GUARDS
    if (unlikely(tableLog > (u32)(dtd.maxTableLog+1))) {
        abort();
    }
#endif
        dtd.tableType = 0;
        dtd.tableLog = (u8)tableLog;
        // memcpy(DTable, &dtd, sizeof(dtd));
        u32 value = dtd.maxTableLog | (dtd.tableType << 8) | (dtd.tableLog << 16);
        *DTable = value;
    }

    /* Calculate starting value for each rank */
    {
        u32 n, nextRankStart = 0;
        for (n=1; n<tableLog+1; n++) {
            u32 const current = nextRankStart;
            nextRankStart += (rankVal[n] << (n-1));
            rankVal[n] = current;
        }
    }

    /* fill DTable */
    {
        u32 n;
        size_t const nEnd = nbSymbols;
        for (n=0; n<nEnd; n++) {
            size_t const w = huffWeight[n];
            size_t const length = (1 << w) >> 1;
            size_t const uStart = rankVal[w];
            size_t const uEnd = uStart + length;
            size_t u;
            struct HUF_DEltX1 D;
            D.byte = (u8)n;
            D.nbBits = (u8)(tableLog + 1 - w);
            rankVal[w] = (u32)uEnd;
            // TODO remove softcache
            if (length < 4) {
                /* Use length in the loop bound so the compiler knows it is short. */
                for (u = 0; u < length; ++u)
                    dt[uStart + u] = D;
            } else {
                /* Unroll the loop 4 times, we know it is a power of 2. */
                for (u = uStart; u < uEnd; u += 4) {
                    dt[u + 0] = D;
                    dt[u + 1] = D;
                    dt[u + 2] = D;
                    dt[u + 3] = D;
                }
            }
        }
    }

    return iSize;
}

static u8 HUF_decodeSymbolX1(struct BIT_DStream* Dstream, const __mram_ptr struct HUF_DEltX1* dt, const u32 dtLog) {
    size_t const val = BIT_lookBitsFast(Dstream, dtLog); /* note : dtLog >= 1 */
    u8 const c = dt[val].byte;
    BIT_skipBits(Dstream, dt[val].nbBits);
    return c;
}

static __dma_aligned char writeCacheBuffers[NR_TASKLETS][4][MRAM_CACHE_SIZE];

struct HUF_WriteCache {
    s32 idx;
    char *buffer;
    char *bufferEnd;
    __mram_ptr u8 *mramPtr;
};

static void HUF_initWriteCache(struct HUF_WriteCache *cache, __mram_ptr void *ptr, char *buffer) {
    u32 off = (uintptr_t)ptr & DMA_OFF_MASK;

    cache->buffer = buffer;
    cache->bufferEnd = ((char *)cache->buffer) + MRAM_CACHE_SIZE;
    cache->mramPtr = ptr;
    cache->idx = -MRAM_CACHE_SIZE + off;

    if (off != 0) {
        mram_read(ptr, cache->buffer, DMA_ALIGNED(off));
    }
}

static void HUF_storeSymbol(struct HUF_WriteCache *cache, u8 symbol) {
    cache->mramPtr++;
    cache->bufferEnd[cache->idx++] = symbol;

    if (cache->idx == 0) {
        mram_write(cache->buffer, cache->mramPtr - MRAM_CACHE_SIZE, MRAM_CACHE_SIZE);
        cache->idx = -MRAM_CACHE_SIZE;
    }
}

static void HUF_flushWriteCache(struct HUF_WriteCache *cache) {
    u32 idx = MRAM_CACHE_SIZE + cache->idx;
    if (idx != 0) {
        u32 off = idx & DMA_OFF_MASK;
        if (off != 0) {
            u64 buffer;
            mram_read(cache->mramPtr, &buffer, 8);
            memcpy(((char*)cache->buffer) + idx, ((u8*)&buffer) + off, 8 - off);
            mram_write(cache->buffer, cache->mramPtr - idx, DMA_ALIGNED(idx));
        } else {
            mram_write(cache->buffer, cache->mramPtr - idx, idx);
        }
    }
}

#define HUF_DECODE_SYMBOLX1_0(ptr, DStreamPtr) \
    HUF_storeSymbol(ptr, HUF_decodeSymbolX1(DStreamPtr, dt, dtLog))

#define HUF_DECODE_SYMBOLX1_1(ptr, DStreamPtr)  \
    if (MEM_64bits() || (HUF_TABLELOG_MAX<=12)) \
        HUF_DECODE_SYMBOLX1_0(ptr, DStreamPtr)

#define HUF_DECODE_SYMBOLX1_2(ptr, DStreamPtr) \
    if (MEM_64bits()) \
        HUF_DECODE_SYMBOLX1_0(ptr, DStreamPtr)

static void HUF_decodeStreamX1(struct HUF_WriteCache* p, struct BIT_DStream* const bitDPtr, __mram_ptr u8* const pEnd, const __mram_ptr struct HUF_DEltX1* const dt, const u32 dtLog) {

    /* up to 4 symbols at a time */
    while ((BIT_reloadDStream(bitDPtr) == BIT_DStream_unfinished) & (p->mramPtr < pEnd-3)) {
        HUF_DECODE_SYMBOLX1_2(p, bitDPtr);
        HUF_DECODE_SYMBOLX1_1(p, bitDPtr);
        HUF_DECODE_SYMBOLX1_2(p, bitDPtr);
        HUF_DECODE_SYMBOLX1_0(p, bitDPtr);
    }

    /* [0-3] symbols remaining */
    if (MEM_32bits()) {
        while ((BIT_reloadDStream(bitDPtr) == BIT_DStream_unfinished) & (p->mramPtr < pEnd)) {
            HUF_DECODE_SYMBOLX1_0(p, bitDPtr);
        }
    }

    /* no more data to retrieve from bitstream, no need to reload */
    while (p->mramPtr < pEnd) {
        HUF_DECODE_SYMBOLX1_0(p, bitDPtr);
    }
}

static size_t HUF_decompress1XUsingDTable(__mram_ptr void *dst, size_t dstSize, struct MramStream *cSrc, size_t cSrcSize, const __mram_ptr u32 *DTable) {
    __mram_ptr u8* op = dst;
    __mram_ptr u8* oend = op + dstSize;
    const __mram_ptr void* dtPtr = DTable + 1;
    const __mram_ptr struct HUF_DEltX1 *const dt = (__mram_ptr struct HUF_DEltX1 *)dtPtr;
    struct BIT_DStream bitD;
    struct DTableDesc const dtd = HUF_getDTableDesc(DTable);
    u32 const dtLog = dtd.tableLog;

    __mram_ptr void* start = (__mram_ptr void*)streamGetMramAddr(cSrc);
    BIT_initDStream(&bitD, start, cSrcSize, bitDStreamBuffers[me()][0]);
    streamSetAt(cSrc, start + cSrcSize);

    struct HUF_WriteCache cache;
    HUF_initWriteCache(&cache, op, writeCacheBuffers[me()][0]);
    HUF_decodeStreamX1(&cache, &bitD, oend, dt, dtLog);
    HUF_flushWriteCache(&cache);

#if USE_DEF_GUARDS
    if (unlikely(!BIT_endOfDStream(&bitD))) {
        abort();
    }
#endif

    return dstSize;
}

static size_t HUF_decompress4XUsingDTable(__mram_ptr void *dst, size_t dstSize, struct MramStream *cSrc, size_t cSrcSize, const __mram_ptr u32 *DTable) {
#if USE_DEF_GUARDS
    if (unlikely(cSrcSize < 10)) {
        /* strict minimum : jump table + 1 byte per stream */
        abort();
    }
#endif

    const u8* const iptr = cSrc->ptr;
    const __mram_ptr u8 *const istart = (__mram_ptr u8*) streamGetMramAddr(cSrc);
    __mram_ptr u8 *const ostart = dst;
    __mram_ptr u8 *const oend = ostart + dstSize;
    __mram_ptr u8 *const olimit = oend - 3;
    const __mram_ptr void* const dtPtr = DTable + 1;
    const __mram_ptr struct HUF_DEltX1* const dt = (const __mram_ptr struct HUF_DEltX1*)dtPtr;

    /* Init */
    struct BIT_DStream bitD1;
    struct BIT_DStream bitD2;
    struct BIT_DStream bitD3;
    struct BIT_DStream bitD4;

    size_t const length1 = WRAM_read16(iptr);
    size_t const length2 = WRAM_read16(iptr + 2);
    size_t const length3 = WRAM_read16(iptr + 4);
    size_t const length4 = cSrcSize - (length1 + length2 + length3 + 6);

    const __mram_ptr u8* const istart1 = istart + 6;  /* jumpTable */
    const __mram_ptr u8* const istart2 = istart1 + length1;
    const __mram_ptr u8* const istart3 = istart2 + length2;
    const __mram_ptr u8* const istart4 = istart3 + length3;

	const size_t segmentSize = (dstSize+3) / 4;
    __mram_ptr u8* const opStart2 = ostart + segmentSize;
    __mram_ptr u8* const opStart3 = opStart2 + segmentSize;
    __mram_ptr u8* const opStart4 = opStart3 + segmentSize;

    __mram_ptr u8* op1 = ostart;
    __mram_ptr u8* op2 = opStart2;
    __mram_ptr u8* op3 = opStart3;
    __mram_ptr u8* op4 = opStart4;
    struct DTableDesc const dtd = HUF_getDTableDesc(DTable);
    u32 const dtLog = dtd.tableLog;
    u32 endSignal = 1;

#if USE_DEF_GUARDS
    /* overflow */
    if (unlikely(length4 > cSrcSize)) {
        abort();
    }
#endif

    struct HUF_WriteCache c1;
    struct HUF_WriteCache c2;
    struct HUF_WriteCache c3;
    struct HUF_WriteCache c4;
    HUF_initWriteCache(&c1, op1, writeCacheBuffers[me()][0]);
    HUF_initWriteCache(&c2, op2, writeCacheBuffers[me()][1]);
    HUF_initWriteCache(&c3, op3, writeCacheBuffers[me()][2]);
    HUF_initWriteCache(&c4, op4, writeCacheBuffers[me()][3]);

    BIT_initDStream(&bitD1, istart1, length1, bitDStreamBuffers[me()][0]);
    BIT_initDStream(&bitD2, istart2, length2, bitDStreamBuffers[me()][1]);
    BIT_initDStream(&bitD3, istart3, length3, bitDStreamBuffers[me()][2]);
    BIT_initDStream(&bitD4, istart4, length4, bitDStreamBuffers[me()][3]);
    streamSetAt(cSrc, istart + cSrcSize);

    /* up to 16 symbols per loop (4 symbols per stream) in 64-bit mode */
    for ( ; (endSignal) & (op4 < olimit) ; ) {
        HUF_DECODE_SYMBOLX1_2(&c1, &bitD1);
        HUF_DECODE_SYMBOLX1_2(&c2, &bitD2);
        HUF_DECODE_SYMBOLX1_2(&c3, &bitD3);
        HUF_DECODE_SYMBOLX1_2(&c4, &bitD4);
        HUF_DECODE_SYMBOLX1_1(&c1, &bitD1);
        HUF_DECODE_SYMBOLX1_1(&c2, &bitD2);
        HUF_DECODE_SYMBOLX1_1(&c3, &bitD3);
        HUF_DECODE_SYMBOLX1_1(&c4, &bitD4);
        HUF_DECODE_SYMBOLX1_2(&c1, &bitD1);
        HUF_DECODE_SYMBOLX1_2(&c2, &bitD2);
        HUF_DECODE_SYMBOLX1_2(&c3, &bitD3);
        HUF_DECODE_SYMBOLX1_2(&c4, &bitD4);
        HUF_DECODE_SYMBOLX1_0(&c1, &bitD1);
        HUF_DECODE_SYMBOLX1_0(&c2, &bitD2);
        HUF_DECODE_SYMBOLX1_0(&c3, &bitD3);
        HUF_DECODE_SYMBOLX1_0(&c4, &bitD4);
        endSignal &= BIT_reloadDStreamFast(&bitD1) == BIT_DStream_unfinished;
        endSignal &= BIT_reloadDStreamFast(&bitD2) == BIT_DStream_unfinished;
        endSignal &= BIT_reloadDStreamFast(&bitD3) == BIT_DStream_unfinished;
        endSignal &= BIT_reloadDStreamFast(&bitD4) == BIT_DStream_unfinished;
    }

    /* finish bitStreams one by one */
    HUF_decodeStreamX1(&c1, &bitD1, opStart2, dt, dtLog);
    HUF_decodeStreamX1(&c2, &bitD2, opStart3, dt, dtLog);
    HUF_decodeStreamX1(&c3, &bitD3, opStart4, dt, dtLog);
    HUF_decodeStreamX1(&c4, &bitD4, oend,     dt, dtLog);

    HUF_flushWriteCache(&c1);
    HUF_flushWriteCache(&c2);
    HUF_flushWriteCache(&c3);
    HUF_flushWriteCache(&c4);

    /* check */
#if USE_DEF_GUARDS
    u32 const endCheck = BIT_endOfDStream(&bitD1) & BIT_endOfDStream(&bitD2) & BIT_endOfDStream(&bitD3) & BIT_endOfDStream(&bitD4);
    if (unlikely(!endCheck)) {
        abort();
    }
#endif

    /* decoded size */
    return dstSize;
}

static size_t HUF_decompress1X(__mram_ptr u32 *dctx, __mram_ptr void *dst, size_t dstSize, struct MramStream *cSrc, size_t cSrcSize, void *workspace, size_t wkspSize) {
    size_t const hSize = HUF_readTableX1(dctx, cSrc, cSrcSize, workspace, wkspSize);
#if USE_DEF_GUARDS
    if (unlikely(hSize >= cSrcSize)) {
        abort();
    }
#endif
    cSrcSize -= hSize;
    return HUF_decompress1XUsingDTable(dst, dstSize, cSrc, cSrcSize, dctx);
}

static size_t HUF_decompress4X(__mram_ptr u32 *dctx, __mram_ptr void *dst, size_t dstSize, struct MramStream *cSrc, size_t cSrcSize, void *workspace, size_t wkspSize) {
    size_t const hSize = HUF_readTableX1(dctx, cSrc, cSrcSize, workspace, wkspSize);

#if USE_DEF_GUARDS
    if (unlikely(hSize >= cSrcSize)) {
        abort();
    }
#endif

    cSrcSize -= hSize;
    return HUF_decompress4XUsingDTable(dst, dstSize, cSrc, cSrcSize, dctx);
}

static size_t decodeLiteralsBlock(struct FrameContext *ctx, struct MramStream *src, size_t srcSize) {
#if USE_DEF_GUARDS
    if (unlikely(srcSize < MIN_CBLOCK_SIZE)) {
        abort();
    }
#endif
    const u8 *const istart = src->ptr;
    SymbolEncodingType const litEncType = (SymbolEncodingType)(istart[0] & 3);

    switch (litEncType) {
        case set_repeat:
            /* fall-through */
        case set_compressed: {
#if USE_DEF_GUARDS
            if (unlikely(srcSize < 5)) {
                abort();
            }
#endif
            size_t lhSize, litSize, litCSize;
            bool singleStream = false;
            u8 const lhlCode = ((istart[0]) >> 2) & 3;
            u32 lhc = WRAM_read32(istart);
            switch (lhlCode) {
                case 0: case 1: default:
                    singleStream = lhlCode == 0;
                    lhSize = 3;
                    litSize  = (lhc >> 4) & 0x3FF;
                    litCSize = (lhc >> 14) & 0x3FF;
                    break;
                case 2:
                    lhSize = 4;
                    litSize  = (lhc >> 4) & 0x3FFF;
                    litCSize = lhc >> 18;
                    break;
                case 3:
                    lhSize = 5;
                    litSize  = (lhc >> 4) & 0x3FFFF;
                    litCSize = (lhc >> 22) + ((size_t)istart[4] << 10);
                    break;
            }

#if USE_DEF_GUARDS
            if (unlikely(litSize > ZSTD_BLOCKSIZE_MAX)) {
                abort();
            }
            if (unlikely(litCSize + lhSize > srcSize)) {
                abort();
            }
#endif

            streamAdvance(src, lhSize);

            if (litEncType == set_repeat) {
                 if (singleStream) {
                    HUF_decompress1XUsingDTable(ctx->litBuffer, litSize, src, litCSize, ctx->hufPtr);
                } else {
                    HUF_decompress4XUsingDTable(ctx->litBuffer, litSize, src, litCSize, ctx->hufPtr);
                }
            } else {
                if (singleStream) {
                    HUF_decompress1X(ctx->entropy.hufTable, ctx->litBuffer, litSize, src, litCSize, ctx->workspace, sizeof(ctx->workspace));
                } else {
                    HUF_decompress4X(ctx->entropy.hufTable, ctx->litBuffer, litSize, src, litCSize, ctx->workspace, sizeof(ctx->workspace));
                }
            }

            ctx->litPtr = ctx->litBuffer;
            ctx->litSize = litSize;
            if (litEncType == set_compressed) {
                ctx->hufPtr = ctx->entropy.hufTable;
            }
            MRAM_memset(ctx->litBuffer + ctx->litSize, 0, WILDCOPY_OVERLENGTH);
            return litCSize + lhSize;
        }
        case set_basic: {
            size_t litSize, lhSize;
            u8 const lhlCode = ((istart[0]) >> 2) & 3;
            switch(lhlCode) {
                case 0: case 2: default:
                    lhSize = 1;
                    litSize = istart[0] >> 3;
                    break;
                case 1:
                    lhSize = 2;
                    litSize = WRAM_read16(istart) >> 4;
                    break;
                case 3:
                    lhSize = 3;
                    litSize = WRAM_read24(istart) >> 4;
            }


            if (lhSize + litSize + WILDCOPY_OVERLENGTH > srcSize) {
                /* risk reading beyond src buffer with wildcopy */
#if USE_DEF_GUARDS
                if (unlikely(litSize + lhSize > srcSize)) {
                    abort();
                }
#endif
                streamAdvance(src, lhSize);
                __mram_ptr void * start = (__mram_ptr void*) streamGetMramAddr(src);
                MRAM_memcpy(ctx->litBuffer, start, litSize);
                streamSetAt(src, start + litSize);
                ctx->litPtr = ctx->litBuffer;
                ctx->litSize = litSize;
                MRAM_memset(ctx->litBuffer + ctx->litSize, 0, WILDCOPY_OVERLENGTH);
                return lhSize + litSize;
            }

            /* direct reference into compressed stream */
            ctx->litPtr = (__mram_ptr u8*)(streamGetMramAddr(src) + lhSize);
            ctx->litSize = litSize;
            return lhSize + litSize;
        }
        case set_rle: {
            size_t litSize, lhSize;
            u8 const lhlCode = ((istart[0]) >> 2) & 3;
            switch(lhlCode) {
                case 0: case 2: default:
                    lhSize = 1;
                    litSize = istart[0] >> 3;
                    break;
                case 1:
                    lhSize = 2;
                    litSize = WRAM_read16(istart) >> 4;
                    break;
                case 3:
                    lhSize = 3;
                    litSize = WRAM_read24(istart) >> 4;
#if USE_DEF_GUARDS
                    if (unlikely(srcSize < 4)) {
                        abort();
                    }
#endif
                    break;
            }
#if USE_DEF_GUARDS
            if (unlikely(litSize > ZSTD_BLOCKSIZE_MAX)) {
                abort();
            }
#endif
            MRAM_memset(ctx->litBuffer, istart[lhSize], litSize + WILDCOPY_OVERLENGTH);
            ctx->litPtr = ctx->litBuffer;
            ctx->litSize = litSize;
            return lhSize + 1;
        }
        default:
            abort();
    }
}

static void buildSeqTable_rle(__mram_ptr struct SeqSymbol *dt, u32 baseValue, u32 nbAddBits) {
    __mram_ptr void* ptr = dt;
    __mram_ptr struct SeqSymbolHeader* const DTableH = (__mram_ptr struct SeqSymbolHeader*)ptr;
    __mram_ptr struct SeqSymbol* const cell = dt + 1;

    DTableH->tableLog = 0;
    DTableH->fastMode = 0;

    cell->nbBits = 0;
    cell->nextState = 0;
#if USE_DEF_GUARDS
    if (unlikely(nbAddBits >= 255)) {
        abort();
    }
#endif
    cell->nbAdditionalBits = (u8)nbAddBits;
    cell->baseValue = baseValue;
}

static void buildFSETable(__mram_ptr struct SeqSymbol *dt, const short *normalizedCounter, unsigned maxSymbolValue, const u32 *baseValue, const u32 *nbAdditionalBits, unsigned tableLog) {
    __mram_ptr struct SeqSymbol *const tableDecode = dt+1;
    u16 symbolNext[MaxSeq+1];

    u32 const maxSV1 = maxSymbolValue + 1;
    u32 const tableSize = 1 << tableLog;
    u32 highThreshold = tableSize-1;

#if USE_DEF_GUARDS
    /* Sanity Checks */
    if (unlikely(maxSymbolValue > MaxSeq)) {
        abort();
    }
    if (unlikely(tableLog > MaxFSELog)) {
        abort();
    }
#endif

    /* Init, lay down lowprob symbols */
    {
        struct SeqSymbolHeader *DTableH = (struct SeqSymbolHeader *)mramWriteCache[me()];
        DTableH->tableLog = tableLog;
        DTableH->fastMode = 1;
        {
            s16 const largeLimit= (s16)(1 << (tableLog-1));
            u32 s;
            for (s=0; s<maxSV1; s++) {
                if (normalizedCounter[s]==-1) {
                    tableDecode[highThreshold--].baseValue = s;
                    symbolNext[s] = 1;
                } else {
                    if (normalizedCounter[s] >= largeLimit) DTableH->fastMode=0;
#if USE_DEF_GUARDS
                    if (unlikely(normalizedCounter[s]<0)) {
                        abort();
                    }
#endif
                    symbolNext[s] = (u16)normalizedCounter[s];
                }
            }
        }
        // memcpy(dt, &DTableH, sizeof(DTableH));
        mram_write(DTableH, dt, sizeof(*DTableH));
    }

    /* Spread symbols */
    {
        u32 const tableMask = tableSize-1;
        u32 const step = FSE_TABLESTEP(tableSize);
        u32 s, position = 0;
        for (s=0; s<maxSV1; s++) {
            int i;
            for (i=0; i<normalizedCounter[s]; i++) {
                tableDecode[position].baseValue = s;
                position = (position + step) & tableMask;
                while (position > highThreshold) position = (position + step) & tableMask;   /* lowprob area */
            }
        }
#if USE_DEF_GUARDS
        /* position must reach all cells once, otherwise normalizedCounter is incorrect */
        if (unlikely(position != 0)) {
            abort();
        }
#endif
    }

    /* Build Decoding table */
    {
        u32 u;
        for (u=0; u<tableSize; u++) {
            u32 const symbol = tableDecode[u].baseValue;
            u32 const nextState = symbolNext[symbol]++;
            tableDecode[u].nbBits = (u8) (tableLog - BIT_highbit32(nextState) );
            tableDecode[u].nextState = (u16) ( (nextState << tableDecode[u].nbBits) - tableSize);
#if USE_DEF_GUARDS
            if (unlikely(nbAdditionalBits[symbol] >= 255)) {
                abort();
            }
#endif
            tableDecode[u].nbAdditionalBits = (u8)nbAdditionalBits[symbol];
            tableDecode[u].baseValue = baseValue[symbol];
        }
    }
}

static size_t buildSeqTable(__mram_ptr struct SeqSymbol *DTableSpace, const __mram_ptr struct SeqSymbol ** DTablePtr, SymbolEncodingType type, unsigned max, u32 maxLog, struct MramStream *src, size_t srcSize, const u32 *baseValue, const u32 *nbAdditionalBits, const __mram_ptr struct SeqSymbol *defaultTable) {
    switch (type) {
        case set_rle:
#if USE_DEF_GUARDS
            if (unlikely(!srcSize)) {
                abort();
            }
#endif
            {
                u32 const symbol = *(const u8*)src->ptr;
                streamAdvance(src, 1);
                u32 const baseline = baseValue[symbol];
                u32 const nbBits = nbAdditionalBits[symbol];
                buildSeqTable_rle(DTableSpace, baseline, nbBits);
            }
            *DTablePtr = DTableSpace;
            return 1;
        case set_basic:
            *DTablePtr = defaultTable;
            return 0;
        case set_repeat:
            return 0;
        case set_compressed: {
            unsigned tableLog;
            s16 norm[MaxSeq+1];
            size_t const headerSize = FSE_readNCount(norm, &max, &tableLog, src, srcSize);
#if USE_DEF_GUARDS
            if (unlikely(tableLog > maxLog)) {
                abort();
            }
#else
            (void)maxLog;
#endif
            buildFSETable(DTableSpace, norm, max, baseValue, nbAdditionalBits, tableLog);
            *DTablePtr = DTableSpace;
            return headerSize;
        }
        default:
            abort();
    }
}

static size_t decodeSeqHeaders(struct FrameContext *ctx, int* nbSeqPtr, struct MramStream *src, size_t srcSize) {
    const __mram_ptr u8 *const istart = (__mram_ptr u8 *) streamGetMramAddr(src);
    const __mram_ptr u8 *const iend = istart + srcSize;
    const __mram_ptr u8 *imram = istart;
    const u8 *ip = src->ptr;

    int nbSeq;
#if USE_DEF_GUARDS
    /* check */
    if (unlikely(srcSize < MIN_SEQUENCES_SIZE)) {
        abort();
    }
#endif

    /* SeqHead */
    nbSeq = *ip++;
    imram++;
    if (!nbSeq) {
        *nbSeqPtr=0;
#if USE_DEF_GUARDS
        if (unlikely(srcSize != 1)) {
            abort();
        }
#endif
        return 1;
    }
    if (nbSeq > 0x7F) {
        if (nbSeq == 0xFF) {
#if USE_DEF_GUARDS
            if (unlikely(imram+2 > iend)) {
                abort();
            }
#endif
            nbSeq = WRAM_read16(ip) + LONGNBSEQ;
            ip = streamAdvance(src, 3);
            imram+=2;
        } else {
#if USE_DEF_GUARDS
            if (unlikely(imram >= iend)) {
                abort();
            }
#endif
            nbSeq = ((nbSeq-0x80)<<8) + *ip;
            ip = streamAdvance(src, 2);
            imram++;
        }
    }
    *nbSeqPtr = nbSeq;

    /* FSE table descriptors */
#if USE_DEF_GUARDS
    /* minimum possible size: 1 byte for symbol encoding types */
    if (unlikely(imram+1 > iend)) {
        abort();
    }
#endif

    {
        SymbolEncodingType const LLtype = (SymbolEncodingType) (*ip >> 6);
        SymbolEncodingType const OFtype = (SymbolEncodingType) ((*ip >> 4) & 3);
        SymbolEncodingType const MLtype = (SymbolEncodingType) ((*ip >> 2) & 3);

        streamAdvance(src, 1);
        imram++;

        /* Build DTables */
        {
            size_t const llhSize = buildSeqTable(ctx->entropy.LLTable, &ctx->LLTptr, LLtype, MaxLL, LLFSELog, src, iend - imram, LL_base, LL_bits, LL_defaultDTable);
            imram += llhSize;
        }
        {
            size_t const ofhSize = buildSeqTable(ctx->entropy.OFTable, &ctx->OFTptr, OFtype, MaxOff, OffFSELog, src, iend - imram, OF_base, OF_bits, OF_defaultDTable);
            imram += ofhSize;
        }
        {
            size_t const mlhSize = buildSeqTable(ctx->entropy.MLTable, &ctx->MLTptr, MLtype, MaxML, MLFSELog, src, iend - imram, ML_base, ML_bits, ML_defaultDTable);
            imram += mlhSize;
        }
    }

    return imram-istart;
}

struct Seq {
    size_t litLength;
    size_t matchLength;
    size_t offset;
    const u8 *match;
};

struct FseState {
    size_t state;
    const __mram_ptr struct SeqSymbol *table;
};

struct SeqState {
    struct BIT_DStream DStream;
    struct FseState stateLL;
    struct FseState stateOffb;
    struct FseState stateML;
    size_t prevOffset[ZSTD_REP_NUM];
    const u8 *prefixStart;
    const u8 *dictEnd;
    size_t pos;
};

static void initFseState(struct FseState *DStatePtr, struct BIT_DStream *bitD, const __mram_ptr struct SeqSymbol *dt) {
    const __mram_ptr void *ptr = dt;
    const __mram_ptr struct SeqSymbolHeader *const DTableH = ptr;
    DStatePtr->state = BIT_readBits(bitD, DTableH->tableLog);
    BIT_reloadDStream(bitD);
    DStatePtr->table = dt + 1;
}

static void updateFseStateWithDInfo(struct FseState *DStatePtr, struct BIT_DStream *bitD, struct SeqSymbol const DInfo) {
    u32 const nbBits = DInfo.nbBits;
    size_t const lowBits = BIT_readBits(bitD, nbBits);
    DStatePtr->state = DInfo.nextState + lowBits;
}

static struct Seq decodeSequence(struct SeqState *seqState) {
    struct Seq seq;
    struct SeqSymbol const llDInfo = seqState->stateLL.table[seqState->stateLL.state];
    struct SeqSymbol const mlDInfo = seqState->stateML.table[seqState->stateML.state];
    struct SeqSymbol const ofDInfo = seqState->stateOffb.table[seqState->stateOffb.state];
    u32 const llBase = llDInfo.baseValue;
    u32 const mlBase = mlDInfo.baseValue;
    u32 const ofBase = ofDInfo.baseValue;
    u8 const llBits = llDInfo.nbAdditionalBits;
    u8 const mlBits = mlDInfo.nbAdditionalBits;
    u8 const ofBits = ofDInfo.nbAdditionalBits;
    u8 const totalBits = llBits+mlBits+ofBits;

    /* sequence */
    {
        size_t offset;
        if (ofBits > 1) {
#if USE_DEF_GUARDS
            if (unlikely(ofBits > MaxOff)) {
                abort();
            }
#endif
            offset = ofBase + BIT_readBitsFast(&seqState->DStream, ofBits/*>0*/);   /* <= (ZSTD_WINDOWLOG_MAX-1) bits */
            if (MEM_32bits()) {
                BIT_reloadDStream(&seqState->DStream);
            }

            seqState->prevOffset[2] = seqState->prevOffset[1];
            seqState->prevOffset[1] = seqState->prevOffset[0];
            seqState->prevOffset[0] = offset;
        } else {
            u32 const ll0 = (llBase == 0);
            if (likely(ofBits == 0)) {
                if (likely(!ll0)) {
                    offset = seqState->prevOffset[0];
                } else {
                    offset = seqState->prevOffset[1];
                    seqState->prevOffset[1] = seqState->prevOffset[0];
                    seqState->prevOffset[0] = offset;
                }
            } else {
                offset = ofBase + ll0 + BIT_readBitsFast(&seqState->DStream, 1);
                {
                    size_t temp = (offset==3) ? seqState->prevOffset[0] - 1 : seqState->prevOffset[offset];
                    temp += !temp;   /* 0 is not valid; input is corrupted; force offset to 1 */
                    if (offset != 1) {
                        seqState->prevOffset[2] = seqState->prevOffset[1];
                    }
					seqState->prevOffset[1] = seqState->prevOffset[0];
					seqState->prevOffset[0] = offset = temp;
                }
            }
        }
		seq.offset = offset;
    }

	seq.matchLength = mlBase;
    if (mlBits > 0) {
        seq.matchLength += BIT_readBitsFast(&seqState->DStream, mlBits/*>0*/);
    }

    if (MEM_32bits() && (mlBits+llBits >= STREAM_ACCUMULATOR_MIN_32-LONG_OFFSETS_MAX_EXTRA_BITS_32)) {
        BIT_reloadDStream(&seqState->DStream);
    }

    if (MEM_64bits() && unlikely(totalBits >= STREAM_ACCUMULATOR_MIN_64-(LLFSELog+MLFSELog+OffFSELog))) {
        BIT_reloadDStream(&seqState->DStream);
    }

    seq.litLength = llBase;
    if (llBits > 0) {
        seq.litLength += BIT_readBitsFast(&seqState->DStream, llBits/*>0*/);
    }

    if (MEM_32bits()) {
        BIT_reloadDStream(&seqState->DStream);
    }

    updateFseStateWithDInfo(&seqState->stateLL, &seqState->DStream, llDInfo); /* <=  9 bits */
    updateFseStateWithDInfo(&seqState->stateML, &seqState->DStream, mlDInfo); /* <=  9 bits */
    if (MEM_32bits()) {
        BIT_reloadDStream(&seqState->DStream); /* <= 18 bits */
    }
    updateFseStateWithDInfo(&seqState->stateOffb, &seqState->DStream, ofDInfo); /* <=  8 bits */

    return seq;
}

#define COPY8(d,s) { copy8(d,s); d+=8; s+=8; }
#define COPY16(d,s) { copy16(d,s); d+=16; s+=16; }

static inline void copy4(__mram_ptr void* dst, const __mram_ptr void* src) { MRAM_copy4(dst, src); }
static inline void copy8(__mram_ptr void* dst, const __mram_ptr void* src) { MRAM_copy8(dst, src); }
static inline void copy16(__mram_ptr void* dst, const __mram_ptr void* src) { MRAM_copy16(dst, src); }

static void wildcopy(__mram_ptr void *dst, const __mram_ptr void *src, ptrdiff_t length, ZSTD_overlap ovtype) {
    ptrdiff_t diff = dst - src;
    const __mram_ptr u8 *ip = src;
    __mram_ptr u8 *op = dst;
    __mram_ptr u8 *const oend = op + length;

    if (ovtype == ZSTD_overlap_src_before_dst && diff < WILDCOPY_VECLEN) {
        /* Handle short offset copies. */
        do {
            COPY8(op, ip);
        } while (op < oend);
    } else {
        COPY16(op, ip);
        COPY16(op, ip);
        if (op >= oend) return;
        do {
            COPY16(op, ip);
            COPY16(op, ip);
        } while (op < oend);
    }
}

static void overlapCopy8(__mram_ptr u8** op, __mram_ptr u8 const** ip, size_t offset) {
    if (offset < 8) {
        /* close range match, overlap */
        static const u32 dec32table[] = { 0, 1, 2, 1, 4, 4, 4, 4 };   /* added */
        static const int dec64table[] = { 8, 8, 8, 7, 8, 9,10,11 };   /* subtracted */
        int const sub2 = dec64table[offset];
        (*op)[0] = (*ip)[0];
        (*op)[1] = (*ip)[1];
        (*op)[2] = (*ip)[2];
        (*op)[3] = (*ip)[3];
        *ip += dec32table[offset];
        copy4(*op+4, *ip);
        *ip -= sub2;
    } else {
        copy8(*op, *ip);
    }
    *ip += 8;
    *op += 8;
}

static void safecopy(__mram_ptr u8 *op, __mram_ptr u8 *const oend_w, __mram_ptr u8 const* ip, ptrdiff_t length, ZSTD_overlap ovtype) {
    ptrdiff_t const diff = op - ip;
    __mram_ptr u8 *const oend = op + length;

#if USE_DEF_GUARDS
    if (unlikely(!((ovtype == ZSTD_no_overlap && (diff <= -8 || diff >= 8 || op >= oend_w)) ||
                    (ovtype == ZSTD_overlap_src_before_dst && diff >= 0)))) {
        abort();
    }
#endif

    if (length < 8) {
        /* Handle short lengths. */
        while (op < oend) *op++ = *ip++;
        return;
    }
    if (ovtype == ZSTD_overlap_src_before_dst) {
        /* Copy 8 bytes and ensure the offset >= 8 when there can be overlap. */
        overlapCopy8(&op, &ip, diff);
    }

    if (oend <= oend_w) {
        /* No risk of overwrite. */
        wildcopy(op, ip, length, ovtype);
        return;
    }
    if (op <= oend_w) {
        /* Wildcopy until we get close to the end. */
        wildcopy(op, ip, oend_w - op, ovtype);
        ip += oend_w - op;
        op = oend_w;
    }
    /* Handle the leftovers. */
     while (op < oend) *op++ = *ip++;
}

static size_t execSequenceEnd(__mram_ptr u8 *op, __mram_ptr u8 *const oend, struct Seq sequence, const __mram_ptr u8 **litPtr, const __mram_ptr u8* const litLimit, const __mram_ptr u8* const prefixStart, const __mram_ptr u8 * const virtualStart, const __mram_ptr u8* const dictEnd) {
    __mram_ptr u8 *const oLitEnd = op + sequence.litLength;
    size_t const sequenceLength = sequence.litLength + sequence.matchLength;
    __mram_ptr u8 *const oMatchEnd = op + sequenceLength;
    const __mram_ptr u8 *const iLitEnd = *litPtr + sequence.litLength;
    const __mram_ptr u8* match = oLitEnd - sequence.offset;
    __mram_ptr u8 *const oend_w = oend - WILDCOPY_OVERLENGTH;

#if USE_DEF_GUARDS
    /* bounds checks */
    if (unlikely(oLitEnd >= oMatchEnd)) {
        abort();
    }
    if (unlikely(oMatchEnd > oend)) {
        abort();
    }
    if (unlikely(iLitEnd > litLimit)) {
        abort();
    }
#else
    (void) litLimit;
    (void) oMatchEnd;
#endif

    /* copy literals */
    safecopy(op, oend_w, *litPtr, sequence.litLength, ZSTD_no_overlap);
    op = oLitEnd;
    *litPtr = iLitEnd;

    /* copy Match */
    if (sequence.offset > (size_t)(oLitEnd - prefixStart)) {
        /* offset beyond prefix */
#if USE_DEF_GUARDS
        if (unlikely(sequence.offset > (size_t)(oLitEnd - virtualStart))) {
            abort();
        }
#else
        (void) virtualStart;
#endif
        match = dictEnd - (prefixStart-match);
        if (match + sequence.matchLength <= dictEnd) {
            MRAM_memmove(oLitEnd, match, sequence.matchLength);
            return sequenceLength;
        }
        /* span extDict & currentPrefixSegment */
        {
            size_t const length1 = dictEnd - match;
            MRAM_memmove(oLitEnd, match, length1);
            op = oLitEnd + length1;
            sequence.matchLength -= length1;
            match = prefixStart;
        }
    }
    safecopy(op, oend_w, match, sequence.matchLength, ZSTD_overlap_src_before_dst);
    return sequenceLength;
}

static size_t execSequence(__mram_ptr u8 *op, __mram_ptr u8 *const oend, struct Seq sequence, const __mram_ptr u8 **litPtr, const __mram_ptr u8* const litLimit, const __mram_ptr u8* const prefixStart, const __mram_ptr u8* const virtualStart, const __mram_ptr u8* const dictEnd) {
    __mram_ptr u8 *const oLitEnd = op + sequence.litLength;
    size_t const sequenceLength = sequence.litLength + sequence.matchLength;
    __mram_ptr u8 *const oMatchEnd = op + sequenceLength;
    __mram_ptr u8 *const oend_w = oend - WILDCOPY_OVERLENGTH;
    const __mram_ptr u8 *const iLitEnd = *litPtr + sequence.litLength;
    const __mram_ptr u8* match = oLitEnd - sequence.offset;

    /* Errors and uncommon cases handled here. */
#if USE_DEF_GUARDS
    if (unlikely(oLitEnd >= oMatchEnd)) {
        abort();
    }
#endif
    if (unlikely(iLitEnd > litLimit || oMatchEnd > oend_w)) {
        return execSequenceEnd(op, oend, sequence, litPtr, litLimit, prefixStart, virtualStart, dictEnd);
    }

    /* Assumptions (everything else goes into ZSTD_execSequenceEnd()) */
#if USE_DEF_GUARDS
    /* Literal length is in bounds */
    if (unlikely(iLitEnd > litLimit)) {
        abort();
    }
    /* Can wildcopy literals */
    if (unlikely(oLitEnd > oend_w)) {
        abort();
    }
    /* Can wildcopy matches */
    if (unlikely(oMatchEnd > oend_w)) {
        abort();
    }
#endif

    /* Copy Literals:
     * Split out litLength <= 16 since it is nearly always true. +1.6% on gcc-9.
     * We likely don't need the full 32-byte wildcopy.
     */
#if USE_DEF_GUARDS
    if (unlikely(WILDCOPY_OVERLENGTH < 16)) {
        abort();
    }
#endif

    copy16(op, (*litPtr));
    if (unlikely(sequence.litLength) > 16) {
        wildcopy(op+16, (*litPtr)+16, sequence.litLength-16, ZSTD_no_overlap);
    }
    op = oLitEnd;
    *litPtr = iLitEnd;   /* update for next sequence */

    /* Copy Match */
    if (sequence.offset > (size_t)(oLitEnd - prefixStart)) {
        /* offset beyond prefix -> go into extDict */
#if USE_DEF_GUARDS
	    if (unlikely(sequence.offset > (size_t)(oLitEnd - virtualStart))) {
            abort();
	    }
#endif
        match = dictEnd + (match - prefixStart);
        if (match + sequence.matchLength <= dictEnd) {
            MRAM_memmove(oLitEnd, match, sequence.matchLength);
            return sequenceLength;
        }
        /* span extDict & currentPrefixSegment */
        {
            size_t const length1 = dictEnd - match;
            MRAM_memmove(oLitEnd, match, length1);
            op = oLitEnd + length1;
            sequence.matchLength -= length1;
            match = prefixStart;
        }
    }
    /* Match within prefix of 1 or more bytes */
#if USE_DEF_GUARDS
    if (unlikely(op > oMatchEnd)) {
        abort();
    }
    if (unlikely(oMatchEnd > oend_w)) {
        abort();
    }
    if (unlikely(match < prefixStart)) {
        abort();
    }
    if (unlikely(sequence.matchLength < 1)) {
        abort();
    }
#endif

    /* Nearly all offsets are >= WILDCOPY_VECLEN bytes, which means we can use wildcopy
     * without overlap checking.
     */
    if (likely(sequence.offset >= WILDCOPY_VECLEN)) {
        /* We bet on a full wildcopy for matches, since we expect matches to be
         * longer than literals (in general). In silesia, ~10% of matches are longer
         * than 16 bytes.
         */
        wildcopy(op, match, (ptrdiff_t)sequence.matchLength, ZSTD_no_overlap);
        return sequenceLength;
    }
#if USE_DEF_GUARDS
    if (unlikely(sequence.offset >= WILDCOPY_VECLEN)) {
        abort();
    }
#endif

    /* Copy 8 bytes and spread the offset to be >= 8. */
    overlapCopy8(&op, &match, sequence.offset);

    /* If the match length is > 8 bytes, then continue with the wildcopy. */
    if (sequence.matchLength > 8) {
#if USE_DEF_GUARDS
        if (unlikely(op >= oMatchEnd)) {
            abort();
        }
#endif
        wildcopy(op, match, (ptrdiff_t)sequence.matchLength-8, ZSTD_overlap_src_before_dst);
    }
    return sequenceLength;
}

static size_t decompressSequences(struct FrameContext *ctx, __mram_ptr void *dst, size_t maxDstSize, struct MramStream *seqStart, size_t seqSize, int nbSeq) {
    const __mram_ptr u8 *imram = (__mram_ptr u8*) streamGetMramAddr(seqStart);
    const __mram_ptr u8 *const iend = imram + seqSize;
    __mram_ptr u8 *const ostart = dst;
    __mram_ptr u8 *const oend = ostart + maxDstSize;
    __mram_ptr u8 *op = ostart;
    const __mram_ptr u8 *litPtr = ctx->litPtr;
    const __mram_ptr u8* const litEnd = litPtr + ctx->litSize;
    const __mram_ptr u8* const prefixStart = (const __mram_ptr u8*) (ctx->prefixStart);
    const __mram_ptr u8* const vBase = (const __mram_ptr u8*) (ctx->virtualStart);
    const __mram_ptr u8* const dictEnd = (const __mram_ptr u8*) (ctx->dictEnd);

    /* Regen sequences */
    if (nbSeq) {
        struct SeqState seqState;
        for (u32 i = 0; i < ZSTD_REP_NUM; i++) {
            seqState.prevOffset[i] = ctx->entropy.rep[i];
        }

        BIT_initDStream(&seqState.DStream, imram, iend-imram, bitDStreamBuffers[me()][0]);
        streamSetAt(seqStart, iend);

        initFseState(&seqState.stateLL, &seqState.DStream, ctx->LLTptr);
        initFseState(&seqState.stateOffb, &seqState.DStream, ctx->OFTptr);
        initFseState(&seqState.stateML, &seqState.DStream, ctx->MLTptr);

        for ( ; ; ) {
            struct Seq const sequence = decodeSequence(&seqState);
            size_t const oneSeqSize = execSequence(op, oend, sequence, &litPtr, litEnd, prefixStart, vBase, dictEnd);
            BIT_reloadDStream(&(seqState.DStream));

            op += oneSeqSize;

            if (unlikely(!--nbSeq)) {
                break;
            }
        }

        BIT_DStream_status status = BIT_reloadDStream(&seqState.DStream);
#if USE_DEF_GUARDS
        /* check if reached exact end */
        if (unlikely(status < BIT_DStream_completed)) {
            abort();
        }
#else
        (void) status;
#endif
        /* save reps for next block */
        for (u32 i=0; i<ZSTD_REP_NUM; i++) {
            ctx->entropy.rep[i] = (u32)(seqState.prevOffset[i]);
        }
    }

    /* last literal segment */
    {
        size_t const lastLLSize = litEnd - litPtr;
#if USE_DEF_GUARDS
        if (unlikely(lastLLSize > (size_t)(oend-op))) {
            abort();
        }
#endif
        MRAM_memcpy(op, litPtr, lastLLSize);
        op += lastLLSize;
    }

    return op-ostart;
}

static size_t decompressBlock(struct FrameContext *ctx, __mram_ptr void *dst, size_t dstCapacity, struct MramStream *src, size_t srcSize) {
#if USE_DEF_GUARDS
    if (unlikely(srcSize >= ZSTD_BLOCKSIZE_MAX)) {
        abort();
    }
#endif
    /* Decode literals section */
    size_t const litSize = decodeLiteralsBlock(ctx, src, srcSize);
    srcSize -= litSize;

    /* Build Decoding Tables */
    {
        int nbSeq;
        size_t const seqHSize = decodeSeqHeaders(ctx, &nbSeq, src, srcSize);
        srcSize -= seqHSize;

        return decompressSequences(ctx, dst, dstCapacity, src, srcSize, nbSeq);
    }
}

static void decompressBegin(struct FrameContext *ctx) {
    ctx->dictEnd = (__mram_ptr void *)NULL;
    ctx->entropy.hufTable[0] = (u32)((HufLog)*0x0000001); // little endian only
    memcpy(ctx->entropy.rep, repStartValue, sizeof(repStartValue));  /* initial repcodes */
    ctx->LLTptr = ctx->entropy.LLTable;
    ctx->MLTptr = ctx->entropy.MLTable;
    ctx->OFTptr = ctx->entropy.OFTable;
    ctx->hufPtr = ctx->entropy.hufTable;
}

static void checkContinuity(struct FrameContext *ctx, const __mram_ptr void *dst) {
    ctx->virtualStart = dst;
    ctx->prefixStart = dst;
}

static size_t decompress(__mram_ptr void *dst, size_t dstCapacity, __mram_ptr const void *src, size_t srcSize) {
    u32 id = me();
    __mram_ptr u8 * const ostart = dst;
    __mram_ptr u8 * const oend = ostart + dstCapacity;
    __mram_ptr u8 *op = ostart;

    size_t remainingSrcSize = srcSize;
    struct MramStream istream;
    streamInit(&istream, src);

    struct FrameContext ctx;
    ctx.litBuffer = litStorage[id];
    ctx.entropy.LLTable = LLTableStorage[id];
    ctx.entropy.OFTable = OFTableStorage[id];
    ctx.entropy.MLTable = MLTableStorage[id];
    ctx.entropy.hufTable = hufTableStorage[id];

    decompressBegin(&ctx);
    checkContinuity(&ctx, dst);

    /* Frame Header */
    struct FrameHeader frameHeader;
    size_t const frameHeaderSize = getFrameHeader(&frameHeader, istream.ptr);
    streamAdvance(&istream, frameHeaderSize);
    remainingSrcSize -= frameHeaderSize;
    ctx.header = &frameHeader;

    /* Loop on each block */
    bool lastBlock;
    do {
        u32 const blockHeader = WRAM_read24(istream.ptr);
        lastBlock = (blockHeader & 1) != 0;
        BlockType blockType = (BlockType) ((blockHeader >> 1) & 3);
        size_t blockSize = blockHeader >> 3;
        streamAdvance(&istream, ZSTD_BLOCKHEADERSIZE);
        remainingSrcSize -= ZSTD_BLOCKHEADERSIZE;

        size_t decodedSize;
        switch (blockType) {
            case bt_compressed:
                decodedSize = decompressBlock(&ctx, op, oend - op, &istream, blockSize);
                break;
            case bt_raw:
                decodedSize = copyRawBlock(op, oend - op, &istream, blockSize);
                break;
            case bt_rle:
                decodedSize = setRleBlock(op, oend - op, *(istream.ptr), blockSize);
                blockSize = 1;
                streamAdvance(&istream, blockSize);
                break;
            default:
                abort();
        }

        remainingSrcSize -= blockSize;
        op += decodedSize;
    } while (!lastBlock);

    return op - ostart;
}

#define INPUT_CAPACITY (2 << 20)
#define OUTPUT_CAPACITY (2 << 20)

__mram_noinit u8 input[NR_TASKLETS][INPUT_CAPACITY];
__mram_noinit u8 output[NR_TASKLETS][OUTPUT_CAPACITY];

__host size_t inputSize[NR_TASKLETS];
__host size_t resultSize[NR_TASKLETS];
__host uint64_t cycles[NR_TASKLETS];

int main() {
    u32 id = me();
    if (id == 0) {
        perfcounter_config(COUNT_CYCLES, true);
    }
    resultSize[id] = decompress(output[id], OUTPUT_CAPACITY, input[id], inputSize[id]);
    cycles[id] = perfcounter_get();
    return 0;
}
