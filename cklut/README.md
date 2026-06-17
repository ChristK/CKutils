# cklut

An extremely fast, dependency-free C++17 lookup-table library for **dense**
key grids stored on disk — designed for things like statistical / distribution
parameters indexed by `age`, `sex`, `region`, `year`, etc.

The core idea: when every combination of the key columns exists (a dense grid),
you don't need a search at all. The row offset is computed directly from the
keys with integer arithmetic, and the data is read straight from a
memory-mapped file. **Lookups are O(1)** — a few multiplies/adds plus one memory
load, no hashing, no binary search, no branches.

## Highlights

- **Direct-address indexing** over an N-dimensional dense grid (row-major).
- **Memory-mapped** payload: instant startup, works for files larger than RAM,
  zero parsing.
- **Cross-platform** mmap layer (POSIX `mmap` + Windows `CreateFileMapping`).
- **Self-describing files**: string→integer dictionaries and key ranges are
  stored in the manifest, so files are portable across machines/OSes with no
  sidecar mapping to keep in sync.
- **Sharded output**: payload is split into shards no larger than a configurable
  cap (default **100 MB**), aligned so range scans never cross a file.
- **Streaming builders** for **CSV** (dependency-free) and **Parquet** (Apache
  Arrow), with automatic discovery of categories and integer ranges.
- **Range scans** over the innermost dimension are a single contiguous,
  vectorizable `double` array.

## Files

| File                 | Purpose                                                        |
|----------------------|----------------------------------------------------------------|
| `cklut.hpp`          | Core: format, `Reader`, low-level `ShardedWriter`.             |
| `cklut_build.hpp`    | Generic two-pass streaming builder + `CsvRowSource`.          |
| `cklut_parquet.hpp`  | `ParquetRowSource` (requires Apache Arrow + Parquet).         |
| `examples/`          | `build_csv`, `build_parquet`, `query`, `prefetch`.             |

## On-disk format

A table called `dist` is stored as:

- `dist.ckmeta` — small manifest: dimensions (`min`, `size`), per-dimension
  string dictionaries, value count, shard size, shard count.
- `dist.0000.ckdat`, `dist.0001.ckdat`, … — raw `double` payload shards, each
  `≤ max_bytes`.

Rows are laid out **row-major with the last dimension innermost** (stride 1).
**Put the dimension you scan over last.** Keys map to indices as
`index = key - min` for numeric dims, or via the dictionary for string dims.
Shard sizes are whole multiples of the innermost run, so a `scan_inner` range is
always contiguous within one shard.

## Build

```bash
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
# Parquet importer too (needs Arrow + Parquet):
cmake -B build -DCKLUT_PARQUET=ON && cmake --build build
```

Requires a 64-bit target (a 100M-row × 8-double table is ~6.4 GB and needs a
64-bit address space).

## Building a table from CSV

```cpp
#include "cklut_build.hpp"

cklut::CsvRowSource src("params.csv", /*has_header=*/true, ',');
std::vector<cklut::BuildDim> dims = {
    { "sex",    /*is_string=*/true  },   // categories auto-discovered
    { "region", /*is_string=*/true  },
    { "age",    /*is_string=*/false },   // integer range auto-discovered; scan dim LAST
};
std::vector<std::string> values = { "mean", "sd", "shape", "scale" };
cklut::build_lut(src, "dist", dims, values, /*max_bytes=*/100ull*1024*1024);
```

Parquet is identical — swap in `cklut::ParquetRowSource src("params.parquet");`.

If you already know every category and numeric range, fill them into `BuildDim`
(`categories` / `min` + `size`) and the discovery pass is skipped (one pass
instead of two).

## Querying

```cpp
#include "cklut.hpp"

cklut::Reader<4> lut("dist.ckmeta");   // 4 = doubles per row

// Single lookup (mixed string / int keys):
const double* p = lut.at("female", "south", 57);
double mean = p[0], sd = p[1];

// Hot loop: pre-encode categoricals once, then index with ints.
std::uint64_t sex_i = lut.category_index(0, "female");
std::uint64_t reg_i = lut.category_index(1, "south");
for (int age = 30; age < 100; ++age) {
    const double* r = lut.at(sex_i, reg_i, age);   // pure integer math
}

// Contiguous, vectorizable range scan over the innermost dim:
auto s = lut.scan_inner(sex_i, reg_i);
for (std::size_t i = 0; i < s.count; ++i) {
    const double* r = s.ptr + i * s.stride;        // age = first_age + i
}
```

## Memory behaviour & prefetching

By **default, reads are lazy**. The payload shards are memory-mapped, not read:
only the ~4 KB OS page containing a row you actually touch is paged in (and then
cached). The table can be **larger than RAM** — pages are clean and file-backed,
so the kernel evicts them under pressure and re-reads on demand. The only thing
read in full at open time is the tiny `.ckmeta` manifest.

That is ideal for huge tables with sparse or scan-based access. But a
latency-sensitive service that has enough RAM may prefer to pull the whole table
in up front, so steady-state lookups never hit a cold-page fault. Two opt-in
ways to do that, both `const` and thread-safe:

```cpp
cklut::Reader<4> r("dist.ckmeta");   // (1) lazy — default, nothing pre-loaded

r.prefetch();                        // (2) async hint: background readahead,
                                     //     returns immediately (best-effort)

r.prefetch(/*blocking=*/true);       // (3) blocking: whole table resident in
                                     //     RAM before this returns
```

Or eagerly warm at construction (equivalent to opening then `prefetch(true)`):

```cpp
cklut::Reader<4> r("dist.ckmeta", /*warm=*/true);   // fully resident on return
```

Under the hood: the async hint issues `madvise(MADV_WILLNEED)` (POSIX) /
`PrefetchVirtualMemory` (Windows); the blocking form additionally touches one
byte per page to force residency. Cost of a cold blocking warm is just the
sequential read of the file — e.g. ~320 ms for a 760 MB table (~2.4 GB/s);
a subsequent warm is a few ms (already cached). See
[`examples/prefetch.cpp`](examples/prefetch.cpp).

Guidance:
- **Huge table / scan or sparse access** → leave it lazy (the default).
- **Table fits in RAM and you want predictable lookup latency** → `warm=true`
  (or `prefetch(true)`) once at startup.
- **Want to overlap warm-up with other init** → call `prefetch()` (async) early,
  do other work, then start querying.

## Performance notes

- Compile with `-O3 -march=native -DNDEBUG` (`/O2 /DNDEBUG` on MSVC).
- Open a `Reader` **once** and reuse it; never construct per lookup.
- String keys cost a hash probe — keep them out of the innermost loop by caching
  `category_index(...)` and passing ints.
- Non-contiguous numeric keys (e.g. years 2000, 2005, 2010) should be treated as
  categories so the grid stays dense.

## Validation

`cklut` is validated to reproduce `CKutils::lookup_dt` **exactly** (same
direct-address algorithm). See [`validation/`](validation/):

```bash
./validation/run_validation.sh        # PASS, max_abs=0 on 5000 query rows
```

It covers mixed integer/factor keys, non-zero integer minimums, and
non-alphabetical factor level order, on both single-shard and multi-shard
tables. Set `USE_REAL_CKUTILS=1` to check against the real package.

## Benchmarks

See [`bench/`](bench/). On 100M rows × 4 doubles (3.2 GB, 31 shards), single
thread @ 2.8 GHz:

| Access pattern | Cost | Throughput |
|----------------|------|------------|
| Random lookups | ~117 ns | ~8.6 M/s (DRAM-latency bound) |
| Hot lookups    | ~8.5 ns | ~118 M/s |
| Sequential scan| ~6.9 ns/cell | ~145 M cells/s (~4.7 GB/s) |

## License

MIT — see [LICENSE](LICENSE).
