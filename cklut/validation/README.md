# Validation: cklut vs CKutils::lookup_dt

This harness proves that the C++ `cklut` library returns **exactly** the same
values as `CKutils::lookup_dt` for the same dense lookup table and the same
query rows.

## Why they should agree

`lookup_dt` maps a query row to a row of a dense, key-sorted `data.table` by
computing a direct-address index from the key columns:

- key columns `on` are sorted alphabetically, with `year` forced first;
- each key contributes a 0-based index — `value - min` for consecutive-integer
  keys, or `factor_code - 1` for factor keys;
- those indices are combined with row-major strides
  (`cardinality_prod = shift(rev(cumprod(rev(cardinality))), -1, fill = 1)`).

Expanding `lookup_dt`'s `rownum` arithmetic gives, for keys with cardinalities
`c1..cn`:

```
rownum = (s1-1)*c2*c3*..*cn + (s2-1)*c3*..*cn + ... + (s_{n-1}-1)*cn + s_n
       = row_major_index + 1
```

i.e. a 1-based row-major index — **identical** to `cklut`'s layout (row-major,
last dimension innermost). So as long as the cklut dimension order matches `on`
and factor categories are listed in factor-**level** order, the two must agree.

## What the harness does

| File                  | Role                                                            |
|-----------------------|-----------------------------------------------------------------|
| `reference_lookup.R`  | Pure-R faithful port of `lookup_dt`'s index algorithm (oracle). |
| `gen_expected.R`      | Builds a dense lookup table + random queries; writes CSVs.       |
| `validate_cklut.cpp`  | Builds a cklut table, looks up every query, compares to oracle.  |
| `run_validation.sh`   | Runs the whole thing (single-shard **and** multi-shard).         |

The test schema deliberately exercises the tricky cases:
- integer keys with non-zero, non-one minimums (`year` from 2018, `age` from 40);
- factor keys with **non-alphabetical** level order
  (`region = north,south,east,west`; `sex = men,women`);
- value columns that depend on *all* keys, so any wrong index yields a wrong value.

## Run it

```bash
./run_validation.sh
```

Expected tail:

```
-- single-shard table --
checked 5000 query rows | mismatches=0 | max_abs=0 max_rel=0
RESULT: PASS — cklut matches lookup_dt on every row
-- forced multi-shard table (CKLUT_MAX_BYTES=512) --
checked 5000 query rows | mismatches=0 | max_abs=0 max_rel=0
RESULT: PASS — cklut matches lookup_dt on every row
```

Requirements: a C++17 compiler and `Rscript` with `data.table`. The bundled
reference oracle needs nothing else.

## Validate against the REAL function

The bundled oracle is a line-for-line port of `lookup_dt`'s arithmetic, but you
can check against the actual exported function in an environment where CKutils
is installed:

```bash
USE_REAL_CKUTILS=1 ./run_validation.sh
```

`gen_expected.R` then calls `CKutils::lookup_dt(..., merge = FALSE)` instead of
the reference. Results are identical by construction.
