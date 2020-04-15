CC           = clang
NR_TASKLETS ?= 1
USE_DEF_GUARDS ?= 0
CFLAGS       = --target=dpu-upmem-dpurte -Os -Wall -Wextra -Werror -DNR_TASKLETS=$(NR_TASKLETS) -DSTACK_SIZE_DEFAULT=2056 -DUSE_DEF_GUARDS=$(USE_DEF_GUARDS)

DECOMPRESS_SOURCES  = decompress.c
DECOMPRESS_INCLUDES =
DECOMPRESS_PROGRAM  = decompress.dpu

.PHONY: default all clean

default: all

all: $(DECOMPRESS_PROGRAM)

clean:
	$(RM) $(DECOMPRESS_PROGRAM)

$(DECOMPRESS_PROGRAM): $(DECOMPRESS_SOURCES)
	$(CC) $(CFLAGS) $(DECOMPRESS_INCLUDES) $^ -o $@

