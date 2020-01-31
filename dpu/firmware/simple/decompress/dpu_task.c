/*
 * Copyright (c) 2020 - UPMEM
 */

#include <defs.h>
#include <mram.h>
#include <perfcounter.h>
#include <stdint.h>

#include "dpu_decompress.h"

#define INPUT_SIZE (2 << 20)
#define OUTPUT_SIZE (2 << 20)

__host uint32_t inputSize[NR_TASKLETS];
__host uint32_t resultSize[NR_TASKLETS];
__host uint64_t cycles[NR_TASKLETS];

__mram_noinit uint8_t input[NR_TASKLETS][INPUT_SIZE];
__mram_noinit uint8_t output[NR_TASKLETS][OUTPUT_SIZE];

int main() {
  uint8_t id = me();
  if (id == 0) {
    perfcounter_config(COUNT_CYCLES, true);
  }
  resultSize[id] = decompress(output[id], OUTPUT_SIZE, input[id], inputSize[id]);
  cycles[id] = perfcounter_get();

  return 0;
}
