CC           = dpu-upmem-dpurte-clang
NR_TASKLETS  = 1
CFLAGS       = -Oz -flto -Wall -Wextra -Werror -DNR_TASKLETS=$(NR_TASKLETS) -DSTACK_SIZE_DEFAULT=4096

COMMON_SOURCES     = common/dpu_alloc.c
COMMON_INCLUDES    = -Icommon
DECOMPRESS_SOURCES = $(COMMON_SOURCES) decompress/dpu_task.c decompress/dpu_decompress.c
DECOMPRESS_REF_SOURCES = $(COMMON_SOURCES) decompress/dpu_task.c decompress/zstd_decompress.c
DECOMPRESS_INCLUDES = $(COMMON_INCLUDES) -Idecompress
DECOMPRESS_PROGRAM = decompress_mram.dpu
DECOMPRESS_REF_PROGRAM = decompress_wram.dpu

.PHONY: default all clean

default: all

all: $(DECOMPRESS_PROGRAM) $(DECOMPRESS_REF_PROGRAM)

clean:
	$(RM) $(DECOMPRESS_PROGRAM)
	$(RM) $(DECOMPRESS_REF_PROGRAM)

$(DECOMPRESS_PROGRAM): $(DECOMPRESS_SOURCES)
	$(CC) $(CFLAGS) $(DECOMPRESS_INCLUDES) $^ -o $@

$(DECOMPRESS_REF_PROGRAM): $(DECOMPRESS_REF_SOURCES)
	$(CC) $(CFLAGS) $(DECOMPRESS_INCLUDES) -DUSE_REF_FN $^ -o $@

