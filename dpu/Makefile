.PHONY: default all host dpu clean

NR_TASKLETS ?= 15

default: all

all: host dpu

clean:
	$(RM) decompress
	$(MAKE) -C firmware/simple $@

dpu:
	$(MAKE) -C firmware/simple NR_TASKLETS=$(NR_TASKLETS)

host: decompress
	
CFLAGS = -O3 -g -Wall -Wextra -Werror
DPU_OPTS = `dpu-pkg-config --cflags --libs dpu` -DNR_TASKLETS=$(NR_TASKLETS)
ZSTD_OPTS = -I../lib/ -L../lib/ -lzstd 

decompress: host_decompress.c
	$(CC) $^ $(CFLAGS) $(DPU_OPTS) $(ZSTD_OPTS) -DDPU_DECOMPRESS_PROGRAM=\"firmware/simple/decompress.dpu\" -o $@
