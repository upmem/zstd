#include <dpu.h>
#include <dpu_log.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <zstd.h>

#ifndef DPU_DECOMPRESS_PROGRAM
#error DPU_DECOMPRESS_PROGRAM is not defined
#endif

static uint32_t DPU_decompress(const uint8_t* src, uint32_t src_size, uint8_t* dst);

#define SRC_SIZE (1 << 20)
#define DST_SIZE (1 << 20)

static uint8_t src[SRC_SIZE];
static uint8_t dst[DST_SIZE];

int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr, "usage: %s <compressed> <decompressed>\n", argv[0]);
        return EXIT_FAILURE;
    }

    char* input = argv[1];
    char* output = argv[2];

    FILE *fin = fopen(input, "r");

    fseek(fin, 0, SEEK_END);
    uint32_t input_size = ftell(fin);
    fseek(fin, 0, SEEK_SET);

    if (input_size > SRC_SIZE) {
        fprintf(stderr, "input_size is too big (%d > %d)\n", input_size, SRC_SIZE);
        exit(EXIT_FAILURE);
    }

    fread(src, sizeof(*src), input_size, fin);
    fclose(fin);

    uint32_t res_size = DPU_decompress(src, input_size, dst);

    FILE *fout = fopen(output, "w");
    fwrite(dst, sizeof(*dst), res_size, fout);
    fclose(fout);

    return EXIT_SUCCESS;
}

static uint32_t DPU_decompress(const uint8_t* src, uint32_t src_size, uint8_t* dst) {
    struct dpu_set_t dpus;
    struct dpu_set_t dpu;
    uint32_t res_size;
    
    DPU_ASSERT(dpu_alloc(1, NULL, &dpus));

    DPU_FOREACH(dpus, dpu) {
        break;
    }

    DPU_ASSERT(dpu_load(dpu, DPU_DECOMPRESS_PROGRAM, NULL));
    DPU_ASSERT(dpu_copy_to(dpu, "inputSize", 0, &src_size, sizeof(src_size)));
    DPU_ASSERT(dpu_copy_to(dpu, "input", 0, src, (src_size + 3) & ~3));
    dpu_error_t err = dpu_launch(dpu, DPU_SYNCHRONOUS);
    dpu_log_read(dpu, stdout);
    if (err != DPU_OK) {
        fprintf(stderr, "%s:%d(%s): DPU Error: %s\n", __FILE__, __LINE__, __func__, dpu_error_to_string(err));
        exit(EXIT_FAILURE);
    }

    DPU_ASSERT(dpu_copy_from(dpu, "resultSize", 0, &res_size, sizeof(res_size)));
        
    if (res_size > DST_SIZE) {
        fprintf(stderr, "res_size is too big (%d > %d)\n", res_size, DST_SIZE);
        exit(EXIT_FAILURE);
    }
    
    DPU_ASSERT(dpu_copy_from(dpu, "output", 0, dst, (res_size + 3) & ~3));
    uint64_t cycles;
    DPU_ASSERT(dpu_copy_from(dpu, "cycles", 0, &cycles, sizeof(cycles)));
    printf("input  size: %8d B\n", src_size);
    printf("output size: %8d B\n", res_size);
    printf("duration   : %8ld cycles\n", cycles);

    DPU_ASSERT(dpu_free(dpus));

    return res_size;
}
