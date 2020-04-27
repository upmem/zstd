# ZSTD on DPU

Port of ZSTD decompressor for a DPU-based solution.

## Contents

`firmware/` contains the DPU code:

 - `firmware/simple` contains an implementation based on the educational decoder
 - `firmware/lib` contains an implementation based on the ZSTD library code

`host_decompress.c` contains a host harness, decompressing the input file on the DPUs.

Currently, the compression must be done with the host `zstd`.

## How to build

`make` should do the trick.
Setting `DPU_SIMPLE=1` will use the simple implementation instead of the standard one.

## Performance

A DPU can decompress 12 files in parallel. A standard system can contain 1024 DPUs.
The assumed DPU frequency is 267MHz.

| File type  | File size      | DPU duration      | DPU throughput @267MHz | System throughput |
| ---------- | -------------- | ----------------- | ---------------------- | ----------------- |
| Executable | 1028152 B each | 921253408  cycles | 3.58 MB/s              | 3.58 GB/s         |
| Executable | 1660152 B each | 1250792384 cycles | 4.25 MB/s              | 4.25 GB/s         |

## Notes

- The diversity of benchmarks should be extended to have more precive measures on different usecases (eg. small files).
- The host overhead is not taken into account in the current performance measures.
- Performances are limited in various ways:
	- the implementation is currently MRAM-bound (only 0.797 instruction per cycle); this can be hard to improve because of the structures used in the implementation that have to be located in MRAM (notably the 4096B random-access array used to decode symbols)
	- only 12 threads are used (because of the WRAM scarcity), which is the bare minimum when the MRAM is not a limiting factor
	- the dictionary feature is not implemented, which could be an improvement for small files
	- the `lto` feature and the `O2` optimization level are not enabled because of the IRAM size
- The DPU implementation does not support the checksum feature.
- The DPU inputs and outputs are limited to 2GB each, which means that the compression implementation needs to be adapted to limit the frame size.
- The DPU only accepts 12 inputs at a time. For small files, this is not efficient and should be changed to support a list of inputs.
