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
| `examples/`          | `build_csv`, `build_parquet`, `query`.                         |

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

## Performance notes

- Compile with `-O3 -march=native -DNDEBUG` (`/O2 /DNDEBUG` on MSVC).
- Open a `Reader` **once** and reuse it; never construct per lookup.
- String keys cost a hash probe — keep them out of the innermost loop by caching
  `category_index(...)` and passing ints.
- Non-contiguous numeric keys (e.g. years 2000, 2005, 2010) should be treated as
  categories so the grid stays dense.

## License

MIT — see [LICENSE](LICENSE).
