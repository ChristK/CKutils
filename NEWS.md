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

## Bug fixes

* `guess_gamlss()` referenced an undefined `dt` (which resolved to `stats::dt`)
  instead of its local `data.table`, so the function always errored before
  returning. It now completes and returns the predicted variable as documented.

* `gnrt_folder_structure()` built the list of folder paths but never created any
  directories (it returned `NULL` without side effects). It now actually creates
  the documented folder structure under `path`.

## Documentation

* Reviewed and completed the roxygen documentation of all exported functions:
  every exported function now documents its return value (`@return`) and carries
  a runnable `@examples` section (heavier model-fitting helpers use `\dontrun`).
  `R CMD check` reports no documentation problems.

## Testing and coverage

* Greatly expanded the `tinytest` suite to systematically cover the package's
  functions and C++ helpers: the `frXXX` random generators, parquet I/O
  round-trips, the gamlss/polr/reldist helpers, `cklut` (build/lookup/export
  plus the multi-shard and `warm` prefetch paths), and the compiled helpers
  (`shift_bypid*`, `counts`/`tableRcpp`, the distribution validation/`log_p`
  branches, parameter recycling, and clamping). Code that cannot be exercised
  by unit tests — package load/unload hooks, real package-install machinery,
  Windows-only and network-only paths, `requireNamespace` guards for Suggests
  packages, and defensive guards — is marked with `# nocov`. Overall test
  coverage rises to roughly 95%.

## Notes

* CSV export preserves factor *labels* but not factor level order; rebuilding
  from CSV yields factors with sorted levels. Use Parquet (or pass an explicit
  level order) when exact factor-level fidelity matters.
