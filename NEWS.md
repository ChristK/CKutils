# CKutils 0.1.25

## New features

* **`cklut`: a memory-mapped, on-disk lookup table and drop-in for `lookup_dt`.**
  Build a dense lookup table once and query it by reading value columns straight
  from a memory-mapped binary file, so the table can be larger than RAM and
  there is no per-call rebuild. Value columns may be of any type (double,
  integer, logical, factor/character), exactly like `lookup_dt`, and unmatched
  keys return `NA`.

  * `cklut_build()` — build from a `data.table`, CSV, or Parquet source.
  * `cklut_lookup()` — drop-in for `lookup_dt()`; returns a list of typed
    vectors, as a `data.table` (`as.data.table = TRUE`, the default) or merged
    into `tbl` (`merge = TRUE`).
  * `cklut_open()` — open a previously built table (optionally `warm`).
  * `cklut_to_dt()`, `cklut_to_csv()` (via `data.table::fwrite`), and
    `cklut_to_parquet()` — read a table back / export it.

  Correctness is validated against **both** `lookup_dt` and `absorb_dt` on
  mixed-type values and no-match (`NA`) rows. On a warm, in-RAM 1.26M-row table,
  `cklut_lookup()` is roughly 1.4–3x faster than `lookup_dt()` depending on query
  volume; the larger structural wins are larger-than-RAM tables and avoiding the
  per-call lookup-table setup.

  The C++ engine, benchmarks, and validation harness live in `cklut/`.

## Notes

* CSV export preserves factor *labels* but not factor level order; rebuilding
  from CSV yields factors with sorted levels. Use Parquet (or pass an explicit
  level order) when exact factor-level fidelity matters.
