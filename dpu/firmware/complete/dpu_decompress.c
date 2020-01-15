/*
 * Copyright (c) 2020 - UPMEM
 */

#include <mram.h>

#include "zstd.h"

#define INPUT_SIZE (1 << 10)
#define OUTPUT_SIZE (1 << 10)

volatile __dma_aligned uint64_t inputSize;
volatile __dma_aligned uint64_t resultSize;
__dma_aligned uint8_t input[INPUT_SIZE];
__dma_aligned uint8_t output[OUTPUT_SIZE];

int main() {
  resultSize = ZSTD_decompress(output, OUTPUT_SIZE, input, inputSize);

  return 0;
}
