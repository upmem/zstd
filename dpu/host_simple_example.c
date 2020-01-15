#include <dpu.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <zstd.h>

#define DPU_DECOMPRESS_PROGRAM "firmware/complete/decompress.dpu"

static uint32_t compress(const uint8_t* src, uint32_t src_size, uint8_t* dst);
static uint32_t DPU_decompress(const uint8_t* src, uint32_t src_size, uint8_t* dst);

#define SRC_SIZE (1 << 10)
#define TMP_SIZE (1 << 10)
#define DST_SIZE (1 << 10)

static uint8_t src[SRC_SIZE];
static uint8_t tmp[TMP_SIZE];
static uint8_t dst[DST_SIZE];

int main(int argc, char **argv) {
    if (argc != 4) {
        fprintf(stderr, "usage: %s <input> <compressed> <decompressed>\n", argv[0]);
        return EXIT_FAILURE;
    }

    char* input = argv[1];
    char* compressed = argv[2];
    char* output = argv[3];

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

    uint32_t compressed_size = compress(src, input_size, tmp);

    FILE *ftmp = fopen(compressed, "w");
    fwrite(tmp, sizeof(*tmp), compressed_size, ftmp);
    fclose(ftmp);

    uint32_t res_size = DPU_decompress(tmp, compressed_size, dst);

    FILE *fout = fopen(output, "w");
    fwrite(dst, sizeof(*dst), res_size, fout);
    fclose(fout);

    return EXIT_SUCCESS;
}

static uint32_t compress(const uint8_t* src, uint32_t src_size, uint8_t* dst) {
   return ZSTD_compress(dst, TMP_SIZE, src, src_size, ZSTD_greedy); 
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
    DPU_ASSERT(dpu_launch(dpu, DPU_SYNCHRONOUS));

    DPU_ASSERT(dpu_copy_from(dpu, "resultSize", 0, &res_size, sizeof(res_size)));
        
    if (res_size > DST_SIZE) {
        fprintf(stderr, "res_size is too big (%d > %d)\n", res_size, DST_SIZE);
        exit(EXIT_FAILURE);
    }
    
    DPU_ASSERT(dpu_copy_from(dpu, "output", 0, dst, (res_size + 3) & ~3));

    DPU_ASSERT(dpu_free(dpus));

    return res_size;
}
