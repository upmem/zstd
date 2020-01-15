/*
 * Copyright (c) 2020 - UPMEM
 */

#include "dpu_alloc.h"

#include <string.h>

void *malloc(size_t size) {
  return mem_alloc(size);
}

void *calloc(unsigned int nr_members, size_t member_size) {
  size_t size = nr_members * member_size;
  void *res = malloc(size);
  memset(res, 0, size);
  return res;
}

void free(void* ptr) {
  (void) ptr;
}
