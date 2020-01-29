/*
 * Copyright (c) 2020 - UPMEM
 */

#include <mram.h>
#include <perfcounter.h>
#include <stdint.h>

#ifdef USE_REF_FN
#include "zstd_decompress.h"
#else
#include "dpu_decompress.h"
#endif

#ifdef USE_REF_FN
#define INPUT_SIZE (2 << 10)
#define OUTPUT_SIZE (2 << 10)
#else
#define INPUT_SIZE (8 << 20)
#define OUTPUT_SIZE (8 << 20)
#endif

__host uint64_t inputSize;
__host uint64_t resultSize;
__host uint64_t cycles;

#ifdef USE_REF_FN
__host uint8_t input[INPUT_SIZE];
__host uint8_t output[OUTPUT_SIZE];
#else
__mram uint8_t input[INPUT_SIZE];
__mram uint8_t output[OUTPUT_SIZE];
#endif

int main() {
  perfcounter_config(COUNT_CYCLES, true);
#ifdef USE_REF_FN
  resultSize = ZSTD_decompress(output, OUTPUT_SIZE, input, inputSize);
#else
  resultSize = decompress(output, OUTPUT_SIZE, input, inputSize);
#endif
  cycles = perfcounter_get();

  return 0;
}
