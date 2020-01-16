/*
 * Copyright (c) 2020 - UPMEM
 */

#include <mram.h>

#include "zstd_decompress.h"

#define INPUT_SIZE (1 << 10)
#define OUTPUT_SIZE (1 << 10)

__host uint64_t inputSize;
__host uint64_t resultSize;
__host uint8_t input[INPUT_SIZE];
__host uint8_t output[OUTPUT_SIZE];

int main() {
  resultSize = ZSTD_decompress(output, OUTPUT_SIZE, input, inputSize);

  return 0;
}
