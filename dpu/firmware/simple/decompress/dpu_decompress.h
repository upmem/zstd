#include <mram.h>
#include <stddef.h>

#define mram(ptr) __mram_ptr ptr

struct dictionary_t;

size_t decompress(
    mram(void *const) dst, const size_t dst_len, const mram(void *const) src, const size_t src_len);
