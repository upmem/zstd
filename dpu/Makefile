.PHONY: default all host dpu clean

ifeq ($(DPU_SIMPLE),1)
NR_TASKLETS ?= 15
DPU_DIR = firmware/simple
else
NR_TASKLETS ?= 12
DPU_DIR = firmware/lib
endif

default: all

all: host dpu

clean:
	$(RM) decompress
	$(MAKE) -C $(DPU_DIR) $@

dpu:
	$(MAKE) -C $(DPU_DIR) NR_TASKLETS=$(NR_TASKLETS)

host: decompress

CFLAGS = -O3 -g -Wall -Wextra -Werror
DPU_OPTS = `dpu-pkg-config --cflags --libs dpu` -DNR_TASKLETS=$(NR_TASKLETS)
ZSTD_OPTS = -I../lib/ -L../lib/ -lzstd

decompress: host_decompress.c
	$(CC) $^ $(CFLAGS) $(DPU_OPTS) $(ZSTD_OPTS) -DDPU_DECOMPRESS_PROGRAM=\"$(DPU_DIR)/decompress.dpu\" -o $@
