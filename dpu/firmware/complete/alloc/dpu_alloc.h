/*
 * Copyright (c) 2020 - UPMEM
 */

#ifndef ZSTD_DPU_ALLOC_H
#define ZSTD_DPU_ALLOC_H

#include <alloc.h>

void *malloc(size_t size);

void *calloc(unsigned int nr_members, size_t member_size);

void free(void* ptr);

#endif // ZSTD_DPU_ALLOC_H
