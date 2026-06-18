# Benchmark

`bench.cpp` builds a large table and measures three access patterns. Build &
run:

```bash
g++ -std=c++17 -O3 -march=native -DNDEBUG -I.. bench.cpp -o bench
./bench                # default 1000 x 1000 x 100 = 100,000,000 rows (3.2 GB)
./bench 500 500 100    # smaller
```

## Reference results

100M rows × 4 doubles (3.2 GB, 31 shards of ≤100 MB), single thread,
Intel Xeon @ 2.8 GHz, `-O3 -march=native`:

| Regime | ns/op | throughput | bound by |
|--------|-------|------------|----------|
| **A. random lookups**   | ~117 ns/lookup | ~8.6 M/s   | DRAM latency (every key is a cache miss across 3.2 GB) |
| **B. hot lookups**      | ~8.5 ns/lookup | ~118 M/s   | index arithmetic (working set in cache) |
| **C. sequential scan**  | ~6.9 ns/cell   | ~145 M cells/s, ~4.7 GB/s | memory bandwidth |

`bad=0` in regime A confirms every lookup returns the correct row at scale.

## How to read these

- **Random** is the worst case: uniformly random keys over the whole 3.2 GB,
  so each lookup is essentially one DRAM round-trip (~100 ns hardware latency).
  This is memory-bound, not compute-bound — the library is already doing the
  minimum (one address computation + one load).
- **Hot** reflects workloads with locality (e.g. a microsimulation sweeping a
  cohort, repeatedly touching nearby keys): ~8.5 ns ≈ 24 cycles for the full
  key-to-pointer computation.
- **Scan** is the fastest per element and the one to prefer: walking the
  innermost dimension is a contiguous, prefetch-friendly, vectorizable stream.

## Multi-threaded benchmark (optional)

The `Reader` is read-only after construction (const schema/strides/divider and a
read-only memory map), so concurrent `at()` / `scan_inner()` calls from many
threads need **no locking**. `bench_mt.cpp` measures how throughput scales:

```bash
g++ -std=c++17 -O3 -march=native -DNDEBUG -pthread -I.. bench_mt.cpp -o bench_mt
./bench_mt                 # default 100M rows, threads = HW concurrency
./bench_mt 1000 1000 100 8 # explicit size + thread cap
# or via CMake:  cmake -B build -DCKLUT_BENCH_MT=ON && cmake --build build
```

Reference scaling, 4 cores @ 2.8 GHz (speedup vs this benchmark's own 1-thread
baseline):

| Pattern | 1 thread | 4 threads | speedup |
|---------|----------|-----------|---------|
| Random lookups | ~11.8 M/s  | ~54 M/s  | **4.6×** |
| Hot lookups    | ~130 M/s   | ~498 M/s | **3.8×** |
| Sequential scan| ~5.9 GB/s  | ~21 GB/s | **3.6×** |

Random lookups scale **super-linearly**: a single thread cannot keep enough
cache misses in flight to saturate DRAM, so adding threads exposes more
memory-level parallelism and hides latency. Hot and scan scale near-linearly
until they hit core throughput / memory bandwidth limits.

## Optimization notes

- Finding the shard for a global row index originally used a hardware
  integer `divide`+`modulo` per lookup (~20-30 cycles). This is replaced by a
  precomputed magic multiply-shift (`detail::FastDivU64`, libdivide-style), and
  skipped entirely for single-shard tables. That cut the hot path from
  ~11.9 ns to ~8.5 ns (1.4×) and random from ~146 ns to ~117 ns.
- To go faster on **random** access you are fighting DRAM latency, not the
  code. Options: enable transparent/explicit **huge pages** for the mapping
  (fewer TLB misses), batch lookups and software-**prefetch** a few keys ahead,
  or exploit any locality you have by sorting query keys so neighbouring lookups
  share cache lines.
- To go faster on **scans**, prefer `scan_inner` and, if you only consume one
  value column, a structure-of-arrays layout would raise effective bandwidth
  (one parameter per contiguous stream instead of interleaved rows).
- The build writes the whole payload via mmap; a cold build of 3.2 GB is
  ~20 s here (one-time, bounded by disk write + `msync`).
