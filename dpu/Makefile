.PHONY: default all host dpu clean

default: all

all: host dpu

clean:
	$(RM) decompress_wram
	$(RM) decompress_mram
	$(MAKE) -C firmware/simple $@

dpu:
	$(MAKE) -C firmware/simple

host: decompress_wram decompress_mram
	
CFLAGS = -O3 -g -Wall -Wextra -Werror
DPU_OPTS = `dpu-pkg-config --cflags --libs dpu`
ZSTD_OPTS = -I../lib/ -L../lib/ -lzstd 

decompress_wram: host_decompress.c
	$(CC) $(CFLAGS) $(DPU_OPTS) $(ZSTD_OPTS) -DDPU_DECOMPRESS_PROGRAM=\"firmware/simple/decompress_wram.dpu\" $^ -o $@

decompress_mram: host_decompress.c
	$(CC) $(CFLAGS) $(DPU_OPTS) $(ZSTD_OPTS) -DDPU_DECOMPRESS_PROGRAM=\"firmware/simple/decompress_mram.dpu\" $^ -o $@
