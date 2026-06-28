// cklut_r.cpp — R (Rcpp) bindings for the typed cklut lookup-table format.
//
// Exposes three primitives the R layer (R/cklut.R) builds on:
//   cklut_open_cpp(meta)               -> external pointer to a TypedReader
//   cklut_schema_cpp(xp)               -> dim + value metadata as an R list
//   cklut_gather_cpp(xp, rownum)       -> named list of typed R vectors
//   cklut_build_cpp(...)               -> write a typed table from R columns
//
// The lookup itself reuses lookup_dt's exact row-index arithmetic in R, so the
// rows selected are identical to CKutils::lookup_dt by construction; this layer
// only turns those row numbers into typed, NA-correct R vectors read straight
// from the memory-mapped file. NA bit-patterns in the file already equal R's
// (NA_real_, NA_integer_, factor code -> NA), so decoding is a plain copy.
//
// The cklut headers are vendored into src/ (canonical copies live in cklut/).
#include <Rcpp.h>
#include "cklut_typed.h"
#include "cklut_build_typed.h"

using namespace Rcpp;
using cklut::ValType;

// --------------------------------------------------------------------------
// [[Rcpp::export]]
SEXP cklut_open_cpp(std::string meta_path, bool warm = false) {
    XPtr<cklut::TypedReader> p(new cklut::TypedReader(meta_path, warm), true);
    return p;
}

// [[Rcpp::export]]
List cklut_schema_cpp(SEXP xp) {
    XPtr<cklut::TypedReader> r(xp);
    const cklut::TypedSchema& s = r->schema();
    const std::size_t ND = s.dims.size(), NV = s.values.size();

    CharacterVector dim_names(ND);
    LogicalVector   dim_is_string(ND);
    NumericVector   dim_min(ND), dim_size(ND);
    List            dim_cats(ND);
    for (std::size_t i = 0; i < ND; ++i) {
        dim_names[i]     = s.dim_names[i];
        dim_is_string[i] = s.dim_is_string(i);
        dim_min[i]       = (double)s.dims[i].min;
        dim_size[i]      = (double)s.dims[i].size;
        if (s.dim_is_string(i)) dim_cats[i] = wrap(s.dim_cats[i]); else dim_cats[i] = R_NilValue;
    }
    CharacterVector value_names(NV), value_types(NV);
    List value_cats(NV);
    for (std::size_t i = 0; i < NV; ++i) {
        value_names[i] = s.values[i].name;
        value_types[i] = cklut::val_type_name(s.values[i].type);
        if (s.values[i].type == ValType::STR) value_cats[i] = wrap(s.values[i].categories);
        else value_cats[i] = R_NilValue;
    }
    return List::create(
        _["dim_names"] = dim_names, _["dim_is_string"] = dim_is_string,
        _["dim_min"] = dim_min, _["dim_size"] = dim_size, _["dim_cats"] = dim_cats,
        _["n_rows"] = (double)s.n_rows, _["n_shards"] = (int)s.n_shards,
        _["value_names"] = value_names, _["value_types"] = value_types, _["value_cats"] = value_cats);
}

// Compute 1-based grid row numbers for `keys` (a list of key columns already in
// the table's dimension order). NA = no match. This is the cklut analogue of
// lookup_dt's starts_from_1_cpp + index arithmetic, done entirely in C++ so the
// drop-in does not pay an R-level encoding cost.
// [[Rcpp::export]]
NumericVector cklut_rownum_cpp(SEXP xp, List keys) {
    XPtr<cklut::TypedReader> r(xp);
    const cklut::TypedSchema& s = r->schema();
    const std::size_t ND = s.dims.size();
    if ((std::size_t)keys.size() != ND) stop("cklut: wrong number of key columns");

    R_xlen_t n = Rf_xlength(keys[0]);
    NumericVector out(n, 1.0);          // accumulate (1-based)
    std::vector<char> ok(n, 1);

    for (std::size_t d = 0; d < ND; ++d) {
        const std::uint64_t stride = s.strides[d];
        const std::int64_t  size   = s.dims[d].size;
        SEXP col = keys[d];
        if (s.dim_is_string(d)) {
            const std::vector<std::string>& cats = s.dim_cats[d];
            bool use_codes = false;
            if (Rf_isFactor(col)) {
                // Fast path: factor whose levels already match the dim dictionary.
                SEXP lv = Rf_getAttrib(col, R_LevelsSymbol);
                if (Rf_xlength(lv) == (R_xlen_t)cats.size()) {
                    use_codes = true;
                    for (R_xlen_t j = 0; j < Rf_xlength(lv); ++j)
                        if (cats[j] != std::string(CHAR(STRING_ELT(lv, j)))) { use_codes = false; break; }
                }
            }
            if (use_codes) {
                const int* code = INTEGER(col);
                for (R_xlen_t i = 0; i < n; ++i) {
                    int c = code[i];
                    if (c == NA_INTEGER) { ok[i] = 0; }
                    else out[i] += (double)(c - 1) * (double)stride;
                }
            } else {
                // General path: map labels through a dictionary.
                std::unordered_map<std::string, int> m; m.reserve(cats.size() * 2);
                for (int j = 0; j < (int)cats.size(); ++j) m.emplace(cats[j], j);
                CharacterVector cc = Rf_isFactor(col) ? as<CharacterVector>(Rf_asCharacterFactor(col))
                                                      : as<CharacterVector>(col);
                for (R_xlen_t i = 0; i < n; ++i) {
                    if (cc[i] == NA_STRING) { ok[i] = 0; continue; }
                    auto it = m.find(std::string(cc[i]));
                    if (it == m.end()) ok[i] = 0; else out[i] += (double)it->second * (double)stride;
                }
            }
        } else {
            const std::int64_t mn = s.dims[d].min;
            if (TYPEOF(col) == INTSXP) {
                const int* v = INTEGER(col);
                for (R_xlen_t i = 0; i < n; ++i) {
                    if (v[i] == NA_INTEGER) { ok[i] = 0; continue; }
                    std::int64_t idx = (std::int64_t)v[i] - mn;
                    if (idx < 0 || idx >= size) ok[i] = 0; else out[i] += (double)idx * (double)stride;
                }
            } else {
                NumericVector vv = as<NumericVector>(col);
                for (R_xlen_t i = 0; i < n; ++i) {
                    if (ISNAN(vv[i])) { ok[i] = 0; continue; }
                    std::int64_t idx = (std::int64_t)vv[i] - mn;
                    if (idx < 0 || idx >= size) ok[i] = 0; else out[i] += (double)idx * (double)stride;
                }
            }
        }
    }
    for (R_xlen_t i = 0; i < n; ++i) if (!ok[i]) out[i] = NA_REAL;
    return out;
}

// rownum: 1-based row numbers (integer or double), NA = no match -> NA values.
// Returns a named list of typed vectors, one per value column.
// [[Rcpp::export]]
List cklut_gather_cpp(SEXP xp, SEXP rownum) {
    XPtr<cklut::TypedReader> r(xp);
    const cklut::TypedSchema& s = r->schema();
    const std::size_t NV = s.values.size();

    // Normalise rownum to a length-n vector of int64 (-1 == NA).
    IntegerVector ri;
    NumericVector rd;
    bool is_int = TYPEOF(rownum) == INTSXP;
    R_xlen_t n;
    if (is_int) { ri = rownum; n = ri.size(); } else { rd = as<NumericVector>(rownum); n = rd.size(); }
    const std::int64_t n_rows_i = (std::int64_t)s.n_rows;
    auto row_at = [&](R_xlen_t i) -> std::int64_t {
        std::int64_t row;
        if (is_int) { int v = ri[i]; row = (v == NA_INTEGER) ? -1 : (std::int64_t)v - 1; }
        else { double v = rd[i]; row = ISNAN(v) ? -1 : (std::int64_t)v - 1; }
        // Out-of-range row -> NA: row_f64()/record() do no bounds check, so an
        // index >= n_rows would read past the last shard's mmap (OOB read).
        return (row < 0 || row >= n_rows_i) ? -1 : row;
    };

    // Pre-allocate one R vector per value column, then fill row-major so each
    // record (one cache line holding all of a row's values) is fetched ONCE.
    List out(NV);
    CharacterVector nms(NV);
    std::vector<double*> pf(NV, nullptr);   // F64/F32/I64 -> double target
    std::vector<int*>    pi(NV, nullptr);   // I32/LGL/STR -> int target
    for (std::size_t v = 0; v < NV; ++v) {
        nms[v] = s.values[v].name;
        switch (s.values[v].type) {
            case ValType::F64: case ValType::F32: case ValType::I64: {
                NumericVector col(n); pf[v] = REAL(col); out[v] = col; break; }
            case ValType::I32: { IntegerVector col(n); pi[v] = INTEGER(col); out[v] = col; break; }
            case ValType::LGL: { LogicalVector col(n); pi[v] = LOGICAL(col); out[v] = col; break; }
            case ValType::STR: {
                IntegerVector col(n); pi[v] = INTEGER(col);
                col.attr("levels") = wrap(s.values[v].categories);
                col.attr("class")  = "factor";
                out[v] = col; break; }
        }
    }
    const auto& offs = s.offsets;

    // Fast path: an all-f64 table is a contiguous double[] per record, so skip
    // the per-column type switch / offset lookup and copy doubles directly
    // (~the same win at_f64() gives the C++ hot loop; see bench_typed.cpp).
#ifndef CKLUT_NO_F64_FASTPATH
    if (r->all_f64()) {
        for (R_xlen_t i = 0; i < n; ++i) {
            std::int64_t row = row_at(i);
            if (row < 0) { for (std::size_t v = 0; v < NV; ++v) pf[v][i] = NA_REAL; continue; }
            const double* d = r->row_f64((std::uint64_t)row);
            for (std::size_t v = 0; v < NV; ++v) pf[v][i] = d[v];
        }
        out.attr("names") = nms;
        return out;
    }
#endif

    for (R_xlen_t i = 0; i < n; ++i) {
        std::int64_t row = row_at(i);
        if (row < 0) {
            for (std::size_t v = 0; v < NV; ++v) {
                if (pf[v]) pf[v][i] = NA_REAL;
                else pi[v][i] = (s.values[v].type == ValType::LGL) ? NA_LOGICAL : NA_INTEGER;
            }
            continue;
        }
        const unsigned char* rec = r->record((std::uint64_t)row);
        for (std::size_t v = 0; v < NV; ++v) {
            const unsigned char* p = rec + offs[v];
            switch (s.values[v].type) {
                case ValType::F64: { double d; std::memcpy(&d, p, 8); pf[v][i] = d; break; }
                case ValType::F32: { float f; std::memcpy(&f, p, 4); pf[v][i] = std::isnan(f) ? NA_REAL : (double)f; break; }
                case ValType::I64: { std::int64_t x; std::memcpy(&x, p, 8); pf[v][i] = (x == cklut::ck_na_i64) ? NA_REAL : (double)x; break; }
                case ValType::I32: { int x; std::memcpy(&x, p, 4); pi[v][i] = x; break; }                 // INT_MIN==NA
                case ValType::LGL: { int x; std::memcpy(&x, p, 4); pi[v][i] = x; break; }                 // INT_MIN==NA
                case ValType::STR: { std::uint32_t c; std::memcpy(&c, p, 4); pi[v][i] = (c == cklut::ck_na_str) ? NA_INTEGER : (int)c + 1; break; }
            }
        }
    }
    out.attr("names") = nms;
    return out;
}

// Build a typed table from already-encoded R columns.
//   dim_index : list, per dim, a 0-based dense-index IntegerVector (length nrow)
//   value_data: list, per value col, an R vector (double/int/logical/factor)
//   value_types: ValType code per value col
//   value_levels: per value col, CharacterVector of levels (STR) else NULL
// [[Rcpp::export]]
void cklut_build_cpp(std::string out_base,
                     CharacterVector dim_names,
                     LogicalVector   dim_is_string,
                     NumericVector   dim_min,
                     NumericVector   dim_size,
                     List            dim_cats,
                     List            dim_index,
                     CharacterVector value_names,
                     IntegerVector   value_types,
                     List            value_data,
                     List            value_levels,
                     double          max_bytes) {
    const std::size_t ND = dim_names.size(), NV = value_names.size();

    std::vector<std::string> dnames(ND);
    std::vector<cklut::DimSpec> dspecs(ND);
    for (std::size_t i = 0; i < ND; ++i) {
        dnames[i] = as<std::string>(dim_names[i]);
        if (dim_is_string[i]) {
            dspecs[i].categories = as<std::vector<std::string>>(as<CharacterVector>(dim_cats[i]));
        } else {
            dspecs[i].min  = (std::int64_t)dim_min[i];
            dspecs[i].size = (std::int64_t)dim_size[i];
        }
    }
    std::vector<cklut::ValSpec> vspecs(NV);
    for (std::size_t i = 0; i < NV; ++i) {
        vspecs[i].name = as<std::string>(value_names[i]);
        vspecs[i].type = (ValType)value_types[i];
        if (vspecs[i].type == ValType::STR)
            vspecs[i].categories = as<std::vector<std::string>>(as<CharacterVector>(value_levels[i]));
    }

    cklut::TypedWriter w(out_base, dnames, dspecs, vspecs, (std::uint64_t)max_bytes);
    const auto& strides = w.schema().strides;

    // length of the build (rows of the source lookup table)
    R_xlen_t nrow = Rf_xlength(dim_index[0]);
    std::vector<IntegerVector> didx(ND);
    for (std::size_t d = 0; d < ND; ++d) didx[d] = as<IntegerVector>(dim_index[d]);

    const std::uint64_t n_rows = w.rows();
    for (R_xlen_t row = 0; row < nrow; ++row) {
        std::uint64_t idx = 0;
        for (std::size_t d = 0; d < ND; ++d) {
            const int di = didx[d][row];
            // An NA (INT_MIN) or negative dense index would make idx wrap/blow up
            // so w.record(idx) writes outside the mmap (UB / memory corruption).
            if (di < 0)
                stop("cklut_build_cpp: NA or negative key index encountered");
            idx += (std::uint64_t)di * strides[d];
        }
        // Catch any out-of-range positive index before it reaches the mmap.
        if (idx >= n_rows)
            stop("cklut_build_cpp: key index out of range for the dense grid");
        unsigned char* rec = w.record(idx);
        for (std::size_t v = 0; v < NV; ++v) {
            SEXP col = value_data[v];
            switch (vspecs[v].type) {
                case ValType::F64: { double d = REAL(col)[row]; w.set_f64(rec, v, d); break; }
                case ValType::F32: { double d = REAL(col)[row]; w.set_f32(rec, v, ISNA(d) ? std::numeric_limits<float>::quiet_NaN() : (float)d); break; }
                case ValType::I32: { int x = INTEGER(col)[row]; w.set_i32(rec, v, x); break; }   // NA_INTEGER preserved
                case ValType::LGL: { int x = LOGICAL(col)[row]; w.set_i32(rec, v, x); break; }   // NA_LOGICAL preserved
                case ValType::I64: { double d = REAL(col)[row]; w.set_i64(rec, v, ISNAN(d) ? cklut::ck_na_i64 : (std::int64_t)llround(d)); break; }
                case ValType::STR: { int code = INTEGER(col)[row]; w.set_code(rec, v, code == NA_INTEGER ? cklut::ck_na_str : (std::uint32_t)(code - 1)); break; }
            }
        }
    }
}
