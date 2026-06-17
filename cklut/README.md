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
- **Two value models**: a numeric (`double`-only) path optimised for range
  scans, and a **typed** path whose value columns may be any of
  `f64/f32/i32/i64/lgl/str` — a full, general replacement for `CKutils::lookup_dt`.
- **R drop-in** (`cklut_build` / `cklut_lookup`) returning typed, NA-correct
  columns, validated to match `lookup_dt` *and* `absorb_dt` exactly.

## Files

| File                 | Purpose                                                        |
|----------------------|----------------------------------------------------------------|
| `cklut.hpp`              | Core numeric format: `Reader<NV>`, low-level `ShardedWriter`. |
| `cklut_build.hpp`        | Generic two-pass numeric builder + `CsvRowSource`.           |
| `cklut_parquet.hpp`      | Numeric `ParquetRowSource` (requires Apache Arrow).          |
| `cklut_typed.hpp`        | **Typed** format: `TypedReader`, `TypedWriter`, `ValType`.   |
| `cklut_build_typed.hpp`  | Typed builder + `CsvTypedRowSource` + `write_csv` exporter.  |
| `cklut_parquet_typed.hpp`| Typed `ParquetTypedRowSource` (requires Apache Arrow).       |
| `examples/`              | `build_csv[_typed]`, `export_csv`, `build_parquet`, `query`, `prefetch`. |
| `tests/test_typed.cpp`   | Unit tests for the typed format (run via `ctest`).          |

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

## Typed (mixed-type) values — the general `lookup_dt` replacement

`CKutils::lookup_dt` returns value columns of *any* R type (it row-subsets the
lookup table). The numeric path above stores `double` only; the **typed** path
(`cklut_typed.hpp`) stores value columns of any of:

| `ValType` | width | R type on read | NA encoding (equals R's) |
|-----------|------:|----------------|--------------------------|
| `f64`     | 8     | `double`       | `NA_real_` bit-pattern   |
| `f32`     | 4     | `double`       | quiet NaN → `NA_real_`   |
| `i32`     | 4     | `integer`      | `INT_MIN` = `NA_integer_`|
| `i64`     | 8     | `double`       | `INT64_MIN`              |
| `lgl`     | 4     | `logical`      | `INT_MIN` = `NA`         |
| `str`     | 4     | `factor`       | code `-1` → `NA`         |

Each row is a fixed-width **record**; columns live at known byte offsets.
Strings/factors are stored as `int32` dictionary codes (+ a per-column
dictionary in the manifest), so the grid stays fixed-width and O(1). An
**all-`f64` table is byte-identical to the numeric format**, so doubles pay
nothing — the typed `Reader` matches the templated `Reader<NV>` within
measurement noise (see `bench/bench_typed.cpp`).

```cpp
#include "cklut_build_typed.hpp"

cklut::CsvTypedRowSource src("dist.csv", /*has_header=*/true, ',');
std::vector<cklut::TypedDim> dims = {{"year",false},{"region",true}};   // int / factor
std::vector<cklut::TypedVal> vals = {
    {"mu",  cklut::ValType::F64}, {"cnt", cklut::ValType::I32},
    {"grp", cklut::ValType::STR}, {"flag",cklut::ValType::LGL}};
cklut::build_lut_typed(src, "dist", dims, vals);

cklut::TypedReader r("dist.ckmeta");
const unsigned char* rec = r.at(2019, "south");
double mu = r.get_f64(rec, 0);  int cnt = r.get_i32(rec, 1);
cklut::write_csv(r, "dist_roundtrip.csv");     // export back to long CSV
```

Command-line round-trip (dependency-free):

```bash
build_csv_typed dist.csv dist  year=int,region=str  mu=f64,cnt=i32,grp=str,flag=lgl
export_csv      dist.ckmeta  dist_roundtrip.csv
```

## R interface — a drop-in for `lookup_dt`

The package wraps the typed reader/writer so cklut can replace `lookup_dt`
directly. Build once, then query like `lookup_dt`; values come straight off the
memory-mapped file (so the table can exceed RAM and there is no per-call
rebuild). The result is a list of typed vectors, returned as a `data.table`
(`setDT`) or merged into `tbl`:

```r
library(CKutils)
# build from a dense data.table (or a .csv / .parquet path)
ck <- cklut_build(lookup_tbl, "dist", keys = c("year","age","region","sex"))

# drop-in for lookup_dt(tbl, lookup_tbl, merge = ...)
res <- cklut_lookup(tbl, ck, merge = FALSE)         # data.table of value columns
cklut_lookup(tbl, ck, merge = TRUE)                 # add value columns to tbl
cklut_lookup(tbl, ck, as.data.table = FALSE)        # plain list of vectors

cklut_to_csv(ck, "dist.csv")                        # export (fwrite)
cklut_to_parquet(ck, "dist.parquet")                # export (arrow)
```

Value columns of type double, integer, factor and logical are returned exactly
as `lookup_dt` would, including `NA` for keys with no match. Correctness is
checked against **both** `lookup_dt` and `absorb_dt`
(`validation/validate_drop_in.R`, and `inst/tinytest/test-cklut.R` in the
package). On a warm, in-RAM 1.26 M-row table with 5 keys, `cklut_lookup` is
~1.3–1.4× faster than `lookup_dt` (`bench/bench_r.R`); the larger wins are for
larger-than-RAM tables and avoiding the per-call lookup-table setup.

> **CSV note:** CSV stores factor *labels* but not level order, so a
> CSV round-trip rebuilds factors with sorted levels. Use Parquet (or pass an
> explicit level order) when exact factor-level fidelity matters.

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

### How much does lazy cost vs warm?

In **steady state there is no difference** — once a page is resident, lazy and
warm both read it straight from the page cache. The gap is purely the
**first touch of cold pages**. Measured on a 2 GB table (64M rows × 4 doubles),
300k random lookups:

| | first pass | second pass |
|---|-----------:|------------:|
| **Lazy** (cold) | ~3500 ns/lookup (page-faults in ~4 KB each) | ~100 ns/lookup |
| **Warm** (`prefetch(true)` first) | ~95 ns/lookup (after a one-time ~750 ms sequential load) | ~80 ns/lookup |

So a cold *random* first touch is ~35× slower than a cached one here (more on a
real SSD/HDD, where random 4 KB reads are far slower than this host-cached VM).
Warm converts those scattered random reads into a single sequential read, so:

- **Touch most of the table, repeatedly** → warm wins (front-load one fast
  sequential read; every lookup is then cache-speed).
- **Touch only a few rows, once** → lazy wins (you never read the rest).
- **Sequential/scan access even when cold** → lazy is already fine; kernel
  readahead streams pages efficiently (no need to warm).

### Larger than RAM: access-pattern hints

When the table **doesn't fit in RAM** and you do sparse/random lookups, the
enemy is OS readahead: each fault can drag in a large readahead window, so even
scattered lookups pull in far more than they use and thrash the page cache.
`advise_random()` disables readahead for the mapping — each lookup faults in only
the single ~4 KB page it needs:

```cpp
cklut::Reader<4> r("dist.ckmeta");
r.advise_random();     // larger-than-RAM, sparse access: keep the resident set small
// ... do lookups; only touched pages stay resident ...
```

Measured effect (3.2 GB table on a host with an 8 MB readahead window, 300k
random lookups):

| | resident set (RSS) after the lookups |
|---|---:|
| default | **2.98 GB** (readahead pulled in ~the whole file) |
| `advise_random()` | **0.96 GB** (only the touched pages) |

Companions: `advise_sequential()` (maximise readahead + drop-behind for big
scans, streams at bandwidth without growing the resident set) and
`advise_normal()` (reset to the default heuristic). All are POSIX
`madvise` hints and no-ops on Windows.

Guidance:
- **Table larger than RAM, sparse/random access** → `advise_random()`.
- **Big sequential scans** → `advise_sequential()` (or just leave it lazy;
  kernel readahead already streams well).
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
./validation/run_validation.sh         # numeric path: PASS, max_abs=0 on 5000 rows
ctest --test-dir build                 # typed format unit tests (test_typed)
Rscript validation/validate_drop_in.R  # R drop-in vs lookup_dt AND absorb_dt
```

The numeric check covers mixed integer/factor keys, non-zero integer minimums,
and non-alphabetical factor level order, on both single-shard and multi-shard
tables (set `USE_REAL_CKUTILS=1` to check against the real package). The typed
checks additionally cover every value type (`f64/f32/i32/i64/lgl/str`), NA
round-trips, and CSV round-trip; `validate_drop_in.R` confirms `cklut_lookup`
agrees with both `lookup_dt` and `absorb_dt` on a mixed-type table including
no-match (`NA`) rows.

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
