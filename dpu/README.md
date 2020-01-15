# ZSTD on DPU

Port of ZSTD for a DPU-based solution.

## Contents

`firmware/` contains the DPU code:

 - `firmware/simple` contains an implementation based on the educational decoder
 - `firmware/complete` contains an implementation based on the ZSTD library code

`host_simple_example.c` contains a host harnest:

 - compressing is done with the host `zstd` library (need to be built before)
 - decompressing is done with the DPU code

## How to build

`make` should do the trick.

## TODO

 - Make the "complete" implementation compile
 - Use the DPU MRAM to fetch the inputs and store the outputs
 - Use multiple DPU threads
 - Benchmarks
