# Graph Report - .  (2026-06-18)

## Corpus Check
- 137 files · ~183,111 words
- Verdict: corpus is large enough that graph structure adds value.

## Summary
- 994 nodes · 1792 edges · 87 communities (74 shown, 13 thin omitted)
- Extraction: 93% EXTRACTED · 7% INFERRED · 0% AMBIGUOUS · INFERRED: 120 edges (avg confidence: 0.8)
- Token cost: 0 input · 0 output

## Community Hubs (Navigation)
- [[_COMMUNITY_cklut R API|cklut R API]]
- [[_COMMUNITY_gamlss distribution benchmark|gamlss distribution benchmark]]
- [[_COMMUNITY_Parameter recycling helpers (C++)|Parameter recycling helpers (C++)]]
- [[_COMMUNITY_misc R utilities|misc R utilities]]
- [[_COMMUNITY_cklut typed build (C++)|cklut typed build (C++)]]
- [[_COMMUNITY_cklut R bindings (src)|cklut R bindings (src)]]
- [[_COMMUNITY_misc C++ helpers|misc C++ helpers]]
- [[_COMMUNITY_cklut build  CSV source (C++)|cklut build / CSV source (C++)]]
- [[_COMMUNITY_cklut Parquet source (C++)|cklut Parquet source (C++)]]
- [[_COMMUNITY_cklut typed build engine (src)|cklut typed build engine (src)]]
- [[_COMMUNITY_cklut typed Parquet source (C++)|cklut typed Parquet source (C++)]]
- [[_COMMUNITY_DPO distribution + SIMD (C++)|DPO distribution + SIMD (C++)]]
- [[_COMMUNITY_cklut typed reader (C++)|cklut typed reader (C++)]]
- [[_COMMUNITY_SICHELZISICHEL distribution (C++)|SICHEL/ZISICHEL distribution (C++)]]
- [[_COMMUNITY_cklut typed core (C++)|cklut typed core (C++)]]
- [[_COMMUNITY_cklut typed C++ tests|cklut typed C++ tests]]
- [[_COMMUNITY_cklut typed writer (C++)|cklut typed writer (C++)]]
- [[_COMMUNITY_shift_bypid (C++)|shift_bypid* (C++)]]
- [[_COMMUNITY_DEL distribution + SIMD (C++)|DEL distribution + SIMD (C++)]]
- [[_COMMUNITY_cklut typed writer (C++)|cklut typed writer (C++)]]
- [[_COMMUNITY_cklut mmap engine (src)|cklut mmap engine (src)]]
- [[_COMMUNITY_cklut sharded writer (C++)|cklut sharded writer (C++)]]
- [[_COMMUNITY_cklut typed schema (C++)|cklut typed schema (C++)]]
- [[_COMMUNITY_NBI distribution (C++)|NBI distribution (C++)]]
- [[_COMMUNITY_ZINBI distribution (C++)|ZINBI distribution (C++)]]
- [[_COMMUNITY_ZANBI distribution (C++)|ZANBI distribution (C++)]]
- [[_COMMUNITY_cklut sharded writer (C++)|cklut sharded writer (C++)]]
- [[_COMMUNITY_Inst Include Recycling|Inst Include Recycling]]
- [[_COMMUNITY_MN4 distribution (C++)|MN4 distribution (C++)]]
- [[_COMMUNITY_cklut sharded writer (C++)|cklut sharded writer (C++)]]
- [[_COMMUNITY_cklut sharded writer (C++)|cklut sharded writer (C++)]]
- [[_COMMUNITY_RNG distribution generators (R)|RNG distribution generators (R)]]
- [[_COMMUNITY_cklut sharded writer (C++)|cklut sharded writer (C++)]]
- [[_COMMUNITY_Src Lookup Dt|Src Lookup Dt]]
- [[_COMMUNITY_cklut sharded writer (C++)|cklut sharded writer (C++)]]
- [[_COMMUNITY_gamlss distributions (C++)|gamlss distributions (C++)]]
- [[_COMMUNITY_cklut sharded writer (C++)|cklut sharded writer (C++)]]
- [[_COMMUNITY_Check Before Push|Check Before Push]]
- [[_COMMUNITY_Src Scramble Trajectories|Src Scramble Trajectories]]
- [[_COMMUNITY_Examples Build Csv|Examples Build Csv]]
- [[_COMMUNITY_cklut validation harness|cklut validation harness]]
- [[_COMMUNITY_misc R utilities|misc R utilities]]
- [[_COMMUNITY_R Cmd Check|R Cmd Check]]
- [[_COMMUNITY_Package management (R)|Package management (R)]]
- [[_COMMUNITY_R distribution & helper wrappers|R distribution & helper wrappers]]
- [[_COMMUNITY_cklut R benchmark|cklut R benchmark]]
- [[_COMMUNITY_cklut R benchmark|cklut R benchmark]]
- [[_COMMUNITY_cklut R benchmark|cklut R benchmark]]
- [[_COMMUNITY_cklut sharded writer (C++)|cklut sharded writer (C++)]]
- [[_COMMUNITY_R distribution & helper wrappers|R distribution & helper wrappers]]
- [[_COMMUNITY_Home User Ckutils|Home User Ckutils]]
- [[_COMMUNITY_R distribution & helper wrappers|R distribution & helper wrappers]]
- [[_COMMUNITY_Rchk Concept|Rchk Concept]]
- [[_COMMUNITY_R Ckutils Onload|R Ckutils Onload]]
- [[_COMMUNITY_misc R utilities|misc R utilities]]
- [[_COMMUNITY_misc R utilities|misc R utilities]]
- [[_COMMUNITY_R distribution & helper wrappers|R distribution & helper wrappers]]
- [[_COMMUNITY_R distribution & helper wrappers|R distribution & helper wrappers]]
- [[_COMMUNITY_R distribution & helper wrappers|R distribution & helper wrappers]]
- [[_COMMUNITY_R distribution & helper wrappers|R distribution & helper wrappers]]
- [[_COMMUNITY_R distribution & helper wrappers|R distribution & helper wrappers]]
- [[_COMMUNITY_Spelling Runner|Spelling Runner]]

## God Nodes (most connected - your core abstractions)
1. `RcppExport` - 72 edges
2. `SEXP` - 71 edges
3. `TypedReader` - 35 edges
4. `ParquetRowSource` - 32 edges
5. `ParquetTypedRowSource` - 31 edges
6. `TypedWriter` - 28 edges
7. `Reader` - 23 edges
8. `TypedSchema` - 23 edges
9. `uint64_t` - 20 edges
10. `ShardedWriter` - 19 edges

## Surprising Connections (you probably didn't know these)
- `fd/fp/fq/fr distribution naming convention` --references--> `fdBCPEo`  [INFERRED]
  .github/copilot-instructions.md → R/RcppExports.R
- `BCPEo density (fdBCPEo)` --references--> `fdBCPEo`  [INFERRED]
  README.md → R/RcppExports.R
- `cklut rownum == lookup_dt 1-based row-major index` --conceptually_related_to--> `lookup_dt`  [EXTRACTED]
  cklut/validation/README.md → R/lookup_dt.R
- `lookup-table key validity (factor or consecutive 1-based int)` --conceptually_related_to--> `lookup_dt`  [EXTRACTED]
  .github/copilot-instructions.md → R/lookup_dt.R
- `drop-in for lookup_dt` --conceptually_related_to--> `lookup_dt`  [EXTRACTED]
  README.md → R/lookup_dt.R

## Import Cycles
- None detected.

## Hyperedges (group relationships)
- **cklut on-disk lookup pipeline** — r_cklut_cklut_build, r_cklut_cklut_open, r_cklut_cklut_lookup, r_cklut_cklut_to_dt [EXTRACTED 1.00]
- **lookup_dt validation and key helpers** — r_lookup_dt_lookup_dt, r_lookup_dt_is_valid_lookup_tbl, r_lookup_dt_set_lookup_tbl_key [EXTRACTED 1.00]
- **cklut internal schema/value helpers** — r_cklut__cklut_value_type, r_cklut__cklut_type_code, r_cklut__cklut_order_keys, r_cklut__cklut_rownum, r_cklut__cklut_key_grid [INFERRED 0.85]
- **Local package management helpers** —  [EXTRACTED 1.00]
- **dqrng-based discrete distribution generators** —  [EXTRACTED 1.00]
- **dqrng-based continuous Box-Cox generators** —  [EXTRACTED 1.00]
- **BCPEo distribution** — r_rcppexports_fdbcpeo, r_rcppexports_fpbcpeo, r_rcppexports_fqbcpeo [EXTRACTED 1.00]
- **ZINBI/NBI distribution family** — r_rcppexports_fdnbi, r_rcppexports_fpnbi, r_rcppexports_fqnbi, r_rcppexports_frnbi, r_rcppexports_fdzinbi, r_rcppexports_fpzinbi, r_rcppexports_fqzinbi, r_rcppexports_frzinbi [EXTRACTED 1.00]
- **shift_bypid* family** — r_rcppexports_shift_bypidnum, r_rcppexports_shift_bypidint, r_rcppexports_shift_bypidbool, r_rcppexports_shift_bypidstr [EXTRACTED 1.00]
- **Distribution test suite** —  [EXTRACTED 1.00]
- **cklut test coverage** —  [EXTRACTED 1.00]
- **CI pipeline (R-CMD-check + rchk + test-coverage)** — r_cmd_check_concept, rchk_concept, test_coverage_concept [EXTRACTED 1.00]
- **cklut subsystem (docs + R drop-in + validation)** — cklut_readme_drop_in, readme_cklut, validation_validate_drop_in_driver [INFERRED 0.85]
- **cklut validation harness (oracle + generator + drop-in)** — validation_reference_lookup_lookup_dt_ref, validation_gen_expected_driver, validation_validate_drop_in_driver [INFERRED 0.85]

## Communities (87 total, 13 thin omitted)

### Community 0 - "cklut R API"
Cohesion: 0.06
Nodes (51): bench_r.R (cklut vs lookup_dt speed bench), tm() timing helper, three access regimes: random / hot / scan, cklut/bench/README.md (benchmark guide), cklut/CMakeLists.txt (build config), header-only INTERFACE library + ctest test_typed, direct-address O(1) indexing over dense grid, cklut/README.md (C++17 lookup library) (+43 more)

### Community 1 - "gamlss distribution benchmark"
Cohesion: 0.05
Nodes (44): benchmark_distribution, build_param_list, generate_test_data, get_function_names, fdBCPEo, fdBCT, fdBNB, fdDEL (+36 more)

### Community 2 - "Parameter recycling helpers (C++)"
Cohesion: 0.06
Nodes (42): compute_log_c(), NumericVector, fdBCPEo(), fdBCPEo_hlp_f_T(), fpBCPEo(), fqBCPEo(), fqBCPEo_hlp_q_T(), IntegerVector (+34 more)

### Community 3 - "misc R utilities"
Cohesion: 0.07
Nodes (39): agegrp_name, arrow_in, clamp, counts, csv_to_fst, estim_beta_params, get_dropbox_path, get_pcloud_path (+31 more)

### Community 4 - "cklut typed build (C++)"
Cohesion: 0.09
Nodes (32): build_lut_typed(), CsvTypedRowSource, buf_, dcols_, delim_, f_, has_header_, path_ (+24 more)

### Community 5 - "cklut R bindings (src)"
Cohesion: 0.07
Nodes (36): const_string_proxy, cklut_build_cpp(), cklut_gather_cpp(), cklut_open_cpp(), cklut_rownum_cpp(), cklut_schema_cpp(), CharacterVector, IntegerVector (+28 more)

### Community 6 - "misc C++ helpers"
Cohesion: 0.11
Nodes (39): antilogit(), carry_backward_decr(), carry_forward(), carry_forward_incr(), count_if(), IntegerVector, List, LogicalVector (+31 more)

### Community 7 - "cklut build / CSV source (C++)"
Cohesion: 0.11
Nodes (25): build_lut(), BuildDim, categories, column, is_string, min, size, CsvRowSource (+17 more)

### Community 8 - "cklut Parquet source (C++)"
Cohesion: 0.09
Nodes (22): Array, FileReader, Schema, shared_ptr, string, Table, unique_ptr, vector (+14 more)

### Community 9 - "cklut typed build engine (src)"
Cohesion: 0.10
Nodes (30): cklut(), CsvTypedRowSource(), class, namespace, std, next(), restart(), select() (+22 more)

### Community 10 - "cklut typed Parquet source (C++)"
Cohesion: 0.09
Nodes (22): Array, FileReader, Schema, shared_ptr, string, Table, unique_ptr, vector (+14 more)

### Community 11 - "DPO distribution + SIMD (C++)"
Cohesion: 0.13
Nodes (24): CacheEntry, IntegerVector, NumericVector, DPOCache, cache, cache_index, CACHE_SIZE, fdDPO() (+16 more)

### Community 12 - "cklut typed reader (C++)"
Cohesion: 0.13
Nodes (12): get_as_string(), uint32_t, TypedReader, base_, dict_, fdiv_, get_as_string, sch_ (+4 more)

### Community 13 - "SICHEL/ZISICHEL distribution (C++)"
Cohesion: 0.16
Nodes (21): compute_alpha(), compute_cvec(), compute_lbes(), IntegerVector, NumericVector, fcdfSICHEL_scalar(), fdSICHEL(), fpSICHEL() (+13 more)

### Community 14 - "cklut typed core (C++)"
Cohesion: 0.15
Nodes (11): at(), at_f64(), ck_is_na_f64(), ck_na_f64(), encode_one(), int64_t, string_view, T (+3 more)

### Community 15 - "cklut typed C++ tests"
Cohesion: 0.20
Nodes (15): size_t, string, uint64_t, vector, main(), make_rows(), MemSource, cols (+7 more)

### Community 16 - "cklut typed writer (C++)"
Cohesion: 0.16
Nodes (12): string, ValType, vector, typed_rows_per_shard(), val_type_from_name(), val_type_name(), val_width(), ValSpec (+4 more)

### Community 17 - "shift_bypid* (C++)"
Cohesion: 0.14
Nodes (18): DllInfo, RcppExport, _CKutils_lin_interpolation(), _CKutils_shift_bypidBool(), _CKutils_shift_bypidInt(), _CKutils_shift_bypidNum(), _CKutils_shift_bypidStr(), R_init_CKutils() (+10 more)

### Community 18 - "DEL distribution + SIMD (C++)"
Cohesion: 0.19
Nodes (17): IntegerVector, NumericVector, fdDEL(), fdDEL_scalar(), fpDEL(), fpDEL_hlp_fn(), fpDEL_scalar(), fqDEL() (+9 more)

### Community 19 - "cklut typed writer (C++)"
Cohesion: 0.20
Nodes (10): size_t, unique_ptr, TypedWriter, base_, dict_, maps_, sch_, vdict_ (+2 more)

### Community 20 - "cklut mmap engine (src)"
Cohesion: 0.16
Nodes (12): data(), dir_of(), e(), FileMapRW(), class, std, put_u32(), put_u64() (+4 more)

### Community 21 - "cklut sharded writer (C++)"
Cohesion: 0.20
Nodes (14): uint32_t, unique_ptr, unordered_map, vector, put_u32(), put_u64(), rows_per_shard(), shard_path() (+6 more)

### Community 22 - "cklut typed schema (C++)"
Cohesion: 0.13
Nodes (13): TypedSchema, base_name, dim_cats, dim_names, dims, n_rows, n_shards, offsets (+5 more)

### Community 23 - "NBI distribution (C++)"
Cohesion: 0.20
Nodes (14): IntegerVector, NumericVector, fdNBI(), fpNBI(), fpNBI_scalar(), fqNBI(), fqNBI_scalar(), frNBI() (+6 more)

### Community 24 - "ZINBI distribution (C++)"
Cohesion: 0.19
Nodes (14): fdNBI_scalar(), fdZANBI_scalar(), IntegerVector, NumericVector, fdZINBI(), fdZINBI_scalar(), fpZINBI(), fpZINBI_scalar() (+6 more)

### Community 25 - "ZANBI distribution (C++)"
Cohesion: 0.19
Nodes (14): frNBI_scalar(), IntegerVector, NumericVector, fdZANBI(), fpZANBI(), fpZANBI_scalar(), fqZANBI(), frZANBI() (+6 more)

### Community 26 - "cklut sharded writer (C++)"
Cohesion: 0.22
Nodes (8): at(), encode_one(), string_view, T, uint64_t, index(), scan_inner(), Span

### Community 27 - "Inst Include Recycling"
Cohesion: 0.15
Nodes (12): recycle_vectors(), to_numeric_vector(), NumericVector, RecycledVectors3, RecycledVectors4, RecycledVectors5, T, T1 (+4 more)

### Community 28 - "MN4 distribution (C++)"
Cohesion: 0.25
Nodes (13): IntegerVector, NumericVector, fdMN4(), fdMN4_scalar(), fpMN4(), fpMN4_scalar(), fqMN4(), fqMN4_scalar() (+5 more)

### Community 29 - "cklut sharded writer (C++)"
Cohesion: 0.15
Nodes (13): Dim, min, size, int64_t, Schema, base_name, cats, dims (+5 more)

### Community 30 - "cklut sharded writer (C++)"
Cohesion: 0.19
Nodes (4): FileMapRO, base_, len_, size_t

### Community 31 - "RNG distribution generators (R)"
Cohesion: 0.15
Nodes (12): frBCPEo, frBCT, frBNB, frDEL, frDPO, frNBI, frSICHEL, frZABNB (+4 more)

### Community 32 - "cklut sharded writer (C++)"
Cohesion: 0.29
Nodes (6): DimSpec, categories, min, size, dir_of(), string

### Community 33 - "Src Lookup Dt"
Cohesion: 0.21
Nodes (11): DataFrame, CharacterVector, IntegerVector, List, SEXP, dtsubset(), fct_to_int_cpp(), starts_from_1_cpp() (+3 more)

### Community 34 - "cklut sharded writer (C++)"
Cohesion: 0.25
Nodes (9): FastDivU64, ADD, d, magic, more, SHIFT, get_u32(), get_u64() (+1 more)

### Community 35 - "gamlss distributions (C++)"
Cohesion: 0.31
Nodes (10): NumericVector, fdBCT(), fdBCT_normal_approx(), fdBCT_t_cdf(), fdBCT_t_logdens(), fpBCT(), fqBCT(), _CKutils_fdBCT() (+2 more)

### Community 36 - "cklut sharded writer (C++)"
Cohesion: 0.20
Nodes (7): Reader, data_, dict_, fdiv_, sch_, shards_, single_shard_

### Community 37 - "Check Before Push"
Cohesion: 0.22
Nodes (8): CFLAGS, CXX11FLAGS, CXX14FLAGS, CXX17FLAGS, CXX20FLAGS, CXXFLAGS, _R_CHECK_COMPILATION_FLAGS_KNOWN_, check-before-push.sh script

### Community 38 - "Src Scramble Trajectories"
Cohesion: 0.33
Nodes (6): _CKutils_fscramble_trajectories(), LogicalVector, NumericVector, SEXP, fscramble_hlp(), fscramble_trajectories()

### Community 39 - "Examples Build Csv"
Cohesion: 0.40
Nodes (5): string, vector, main(), parse_spec(), pair

### Community 40 - "cklut validation harness"
Cohesion: 0.73
Nodes (5): string, vector, col(), main(), split()

### Community 41 - "misc R utilities"
Cohesion: 0.47
Nodes (5): crossval_gamlss, guess_gamlss, guess_polr, reldist_diagnostics, validate_gamlss

### Community 42 - "R Cmd Check"
Cohesion: 0.40
Nodes (5): GitHub Actions R-CMD-check, R-CMD-check.yml (multi-platform CI), test-coverage (covr + Codecov), test-coverage.yml (covr/Codecov CI), tinytest.R (CKutils test_package runner)

### Community 43 - "Package management (R)"
Cohesion: 0.60
Nodes (4): dependencies, detach_package, installLocalPackage, installLocalPackageIfChanged

### Community 44 - "R distribution & helper wrappers"
Cohesion: 0.40
Nodes (4): shift_bypidBool, shift_bypidInt, shift_bypidNum, shift_bypidStr

### Community 45 - "cklut R benchmark"
Cohesion: 0.67
Nodes (3): main(), secs(), time_point

### Community 46 - "cklut R benchmark"
Cohesion: 0.67
Nodes (3): main(), secs(), time_point

### Community 47 - "cklut R benchmark"
Cohesion: 0.67
Nodes (3): main(), secs(), time_point

### Community 48 - "cklut sharded writer (C++)"
Cohesion: 0.50
Nodes (3): FileMapRW, base_, len_

## Knowledge Gaps
- **297 isolated node(s):** `check-before-push.sh script`, `CFLAGS`, `CXXFLAGS`, `CXX11FLAGS`, `CXX14FLAGS` (+292 more)
  These have ≤1 connection - possible missing edges or undocumented components.
- **13 thin communities (<3 nodes) omitted from report** — run `graphify query` to explore isolated nodes.

## Suggested Questions
_Questions this graph is uniquely positioned to answer:_

- **Why does `RcppExport` connect `shift_bypid* (C++)` to `Src Lookup Dt`, `Parameter recycling helpers (C++)`, `gamlss distributions (C++)`, `cklut R bindings (src)`, `misc C++ helpers`, `Src Scramble Trajectories`, `DPO distribution + SIMD (C++)`, `SICHEL/ZISICHEL distribution (C++)`, `DEL distribution + SIMD (C++)`, `NBI distribution (C++)`, `ZINBI distribution (C++)`, `ZANBI distribution (C++)`, `MN4 distribution (C++)`?**
  _High betweenness centrality (0.030) - this node is a cross-community bridge._
- **Why does `SEXP` connect `misc C++ helpers` to `Src Lookup Dt`, `Parameter recycling helpers (C++)`, `gamlss distributions (C++)`, `cklut R bindings (src)`, `Src Scramble Trajectories`, `DPO distribution + SIMD (C++)`, `SICHEL/ZISICHEL distribution (C++)`, `shift_bypid* (C++)`, `DEL distribution + SIMD (C++)`, `NBI distribution (C++)`, `ZINBI distribution (C++)`, `ZANBI distribution (C++)`, `MN4 distribution (C++)`?**
  _High betweenness centrality (0.030) - this node is a cross-community bridge._
- **Why does `TypedRowSource` connect `cklut typed build (C++)` to `cklut typed Parquet source (C++)`, `cklut typed C++ tests`?**
  _High betweenness centrality (0.026) - this node is a cross-community bridge._
- **What connects `check-before-push.sh script`, `CFLAGS`, `CXXFLAGS` to the rest of the system?**
  _297 weakly-connected nodes found - possible documentation gaps or missing edges._
- **Should `cklut R API` be split into smaller, more focused modules?**
  _Cohesion score 0.05727644652250146 - nodes in this community are weakly interconnected._
- **Should `gamlss distribution benchmark` be split into smaller, more focused modules?**
  _Cohesion score 0.05323653962492438 - nodes in this community are weakly interconnected._
- **Should `Parameter recycling helpers (C++)` be split into smaller, more focused modules?**
  _Cohesion score 0.05782312925170068 - nodes in this community are weakly interconnected._