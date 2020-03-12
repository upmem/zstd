CC           = dpu-upmem-dpurte-clang
NR_TASKLETS ?= 1
USE_DEF_GUARDS ?= 0
CFLAGS       = -O2 -flto -Wall -Wextra -Werror -DNR_TASKLETS=$(NR_TASKLETS) -DSTACK_SIZE_DEFAULT=2048 -DUSE_DEF_GUARDS=$(USE_DEF_GUARDS)

DECOMPRESS_SOURCES = decompress/dpu_task.c decompress/dpu_decompress.c
DECOMPRESS_INCLUDES = -Idecompress
DECOMPRESS_PROGRAM = decompress.dpu

.PHONY: default all clean

default: all

all: $(DECOMPRESS_PROGRAM)

clean:
	$(RM) $(DECOMPRESS_PROGRAM)

$(DECOMPRESS_PROGRAM): $(DECOMPRESS_SOURCES)
	$(CC) $(CFLAGS) $(DECOMPRESS_INCLUDES) $^ -o $@

