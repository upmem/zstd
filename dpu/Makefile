.PHONY: default all host dpu clean

default: all

all: host dpu

clean:
	$(RM) decompress_simple_example
	$(MAKE) -C firmware/simple $@

dpu:
	$(MAKE) -C firmware/simple

host: decompress_simple_example
	
CFLAGS = -O3 -g -Wall -Wextra -Werror
DPU_OPTS = `dpu-pkg-config --cflags --libs dpu`
ZSTD_OPTS = -I../lib/ -L../lib/ -lzstd 

decompress_simple_example: host_simple_example.c
	$(CC) $(CFLAGS) $(DPU_OPTS) $(ZSTD_OPTS) $^ -o $@ 
