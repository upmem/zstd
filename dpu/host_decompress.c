#include <dpu.h>
#include <dpu_log.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#ifndef DPU_DECOMPRESS_PROGRAM
#error DPU_DECOMPRESS_PROGRAM is not defined
#endif

static uint32_t DPU_decompress(const uint8_t *src, uint32_t src_size, uint8_t *dst);

#define SRC_SIZE (2 << 20)
#define DST_SIZE (2 << 20)

static uint8_t src[SRC_SIZE];
static uint8_t dst[DST_SIZE];

int main(int argc, char **argv)
{
    if (argc != 3) {
        fprintf(stderr, "usage: %s <compressed> <decompressed>\n", argv[0]);
        return EXIT_FAILURE;
    }

    char *input = argv[1];
    char *output = argv[2];

    FILE *fin = fopen(input, "r");
    if (fin == NULL) {
        fprintf(stderr, "could not open file '%s'\n", input);
        return EXIT_FAILURE;
    }

    fseek(fin, 0, SEEK_END);
    uint32_t input_size = ftell(fin);
    fseek(fin, 0, SEEK_SET);

    if (input_size > SRC_SIZE) {
        fprintf(stderr, "input_size is too big (%d > %d)\n", input_size, SRC_SIZE);
        return EXIT_FAILURE;
    }

    size_t fread_res = fread(src, sizeof(*src), input_size, fin);
    fclose(fin);
    if (fread_res != input_size) {
        fprintf(stderr, "could not read file '%s'\n", input);
    }

    uint32_t res_size = DPU_decompress(src, input_size, dst);

    FILE *fout = fopen(output, "w");
    if (fout == NULL) {
        fprintf(stderr, "could not open file '%s'\n", output);
        return EXIT_FAILURE;
    }

    size_t fwrite_res = fwrite(dst, sizeof(*dst), res_size, fout);
    fclose(fout);
    if (fwrite_res != res_size) {
        fprintf(stderr, "could not write file '%s'\n", output);
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}

static uint32_t DPU_decompress(const uint8_t *src, uint32_t src_size, uint8_t *dst)
{
    struct dpu_set_t dpus;
    struct dpu_set_t dpu;
    uint32_t res_size;

    DPU_ASSERT(dpu_alloc(1, NULL, &dpus));

    DPU_FOREACH (dpus, dpu) {
        break;
    }

    DPU_ASSERT(dpu_load(dpu, DPU_DECOMPRESS_PROGRAM, NULL));
    for (uint32_t each_tasklet = 0; each_tasklet < NR_TASKLETS; ++each_tasklet) {
        DPU_ASSERT(dpu_copy_to(dpu, "inputSize", each_tasklet * sizeof(src_size), &src_size, sizeof(src_size)));
        DPU_ASSERT(dpu_copy_to(dpu, "input", each_tasklet * SRC_SIZE, src, (src_size + 3) & ~3));
    }
    dpu_error_t err = dpu_launch(dpu, DPU_SYNCHRONOUS);
    dpu_log_read(dpu, stdout);
    if (err != DPU_OK) {
        fprintf(stderr, "%s:%d(%s): DPU Error: %s\n", __FILE__, __LINE__, __func__, dpu_error_to_string(err));
        exit(EXIT_FAILURE);
    }

    for (uint32_t each_tasklet = 0; each_tasklet < NR_TASKLETS; ++each_tasklet) {
        DPU_ASSERT(dpu_copy_from(dpu, "resultSize", each_tasklet * sizeof(res_size), &res_size, sizeof(res_size)));

        if (res_size > DST_SIZE) {
            fprintf(stderr, "res_size is too big (%d > %d)\n", res_size, DST_SIZE);
            exit(EXIT_FAILURE);
        }

        DPU_ASSERT(dpu_copy_from(dpu, "output", each_tasklet * DST_SIZE, dst, (res_size + 3) & ~3));
        uint64_t cycles;
        DPU_ASSERT(dpu_copy_from(dpu, "cycles", each_tasklet * sizeof(cycles), &cycles, sizeof(cycles)));
        printf("input  size: %8d B\n", src_size);
        printf("output size: %8d B\n", res_size);
        printf("duration   : %8ld cycles\n", cycles);
    }

    DPU_ASSERT(dpu_free(dpus));

    return res_size;
}
