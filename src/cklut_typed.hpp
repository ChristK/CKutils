// cklut_typed.hpp — general (mixed-type) cklut: dense, direct-address,
// memory-mapped lookup table whose VALUE columns can be any of
// {double, float, int32, int64, logical, string}, not just double.
//
// This is the "full general replacement" path for CKutils::lookup_dt: lookup_dt
// returns value columns of arbitrary R type (it row-subsets the lookup table),
// so to reach parity cklut must store arbitrary value types too.
//
// Design (see README "Typed format"):
//   * Keys are exactly as in the numeric path: dense N-dim grid, row-major with
//     the last dim innermost; index = (key - min) for numeric dims, or a
//     dictionary code for string/factor dims. O(1) direct address, no search.
//   * Values are stored row-major as a fixed-width RECORD per row: each value
//     column occupies a known byte offset and width inside the record. Strings
//     and factors are stored as int32 dictionary codes (width 4) + a per-column
//     dictionary in the manifest, so the grid stays fixed-width and fast.
//   * NA bit-patterns are chosen to equal R's exactly (NA_real_, NA_integer_,
//     string code -1), so the R binding can read records straight out without
//     any translation.
//   * An ALL-double table has row_bytes = n_values*8 and a record layout byte-
//     identical to the original numeric format, so doubles pay nothing.
//
// Header-only, C++17, cross-platform. Reuses the mmap / varint / fast-divide
// helpers from cklut.hpp.
#pragma once
#include "cklut.hpp"     // detail::FileMapRO/RW, put/get_u32/u64, FastDivU64, shard_path, dir_of
#include <cstring>
#include <limits>
#include <cmath>
#include <string>
#include <vector>
#include <memory>
#include <unordered_map>
#include <fstream>
#include <stdexcept>

namespace cklut {

// --------------------------- value types -----------------------------------
enum class ValType : std::uint8_t {
    F64 = 0,   // double          -> R double
    F32 = 1,   // float           -> R double
    I32 = 2,   // int32           -> R integer
    I64 = 3,   // int64           -> R double (no native R int64)
    LGL = 4,   // int32 0/1/NA    -> R logical
    STR = 5    // int32 dict code -> R factor / character
};

inline std::uint32_t val_width(ValType t) {
    switch (t) {
        case ValType::F64: return 8;
        case ValType::I64: return 8;
        case ValType::F32: return 4;
        case ValType::I32: return 4;
        case ValType::LGL: return 4;
        case ValType::STR: return 4;
    }
    return 8;
}
inline const char* val_type_name(ValType t) {
    switch (t) {
        case ValType::F64: return "f64";
        case ValType::F32: return "f32";
        case ValType::I32: return "i32";
        case ValType::I64: return "i64";
        case ValType::LGL: return "lgl";
        case ValType::STR: return "str";
    }
    return "f64";
}
inline ValType val_type_from_name(const std::string& s) {
    if (s == "f64" || s == "double" || s == "numeric") return ValType::F64;
    if (s == "f32" || s == "float")                    return ValType::F32;
    if (s == "i32" || s == "int" || s == "integer")    return ValType::I32;
    if (s == "i64" || s == "long")                     return ValType::I64;
    if (s == "lgl" || s == "bool" || s == "logical")   return ValType::LGL;
    if (s == "str" || s == "string" || s == "factor" || s == "character") return ValType::STR;
    throw std::runtime_error("cklut: unknown value type '" + s + "'");
}

// R-compatible NA bit-patterns -------------------------------------------------
inline double ck_na_f64() {
    // R's NA_real_ : a quiet NaN whose payload low word is 1954 (0x7A2).
    std::uint64_t bits = 0x7FF00000000007A2ull;
    double d; std::memcpy(&d, &bits, 8); return d;
}
inline bool ck_is_na_f64(double d) {
    if (!std::isnan(d)) return false;
    std::uint64_t bits; std::memcpy(&bits, &d, 8);
    return (bits & 0xFFFFFFFFull) == 1954ull;   // R's NA payload (vs ordinary NaN)
}
constexpr std::int32_t  ck_na_i32 = std::numeric_limits<std::int32_t>::min();   // == R NA_integer_
constexpr std::int64_t  ck_na_i64 = std::numeric_limits<std::int64_t>::min();   // == bit64 NA
constexpr std::uint32_t ck_na_str = 0xFFFFFFFFu;                                // string code = NA

// ------------------------- typed schema ------------------------------------
struct ValSpec {
    std::string              name;
    ValType                  type = ValType::F64;
    std::vector<std::string> categories;   // STR only: dictionary (code order)
};

struct TypedSchema {
    std::vector<std::string>              dim_names;
    std::vector<Dim>                      dims;       // {min,size}
    std::vector<std::vector<std::string>> dim_cats;   // per dim: empty if numeric
    std::vector<ValSpec>                  values;
    std::vector<std::uint32_t>            offsets;     // byte offset of each value col in a record
    std::uint32_t                         row_bytes = 0;
    std::uint64_t                         n_rows = 0, rows_per_shard = 0;
    std::uint32_t                         n_shards = 0;
    std::string                           base_name;
    std::vector<std::uint64_t>            strides;

    bool dim_is_string(std::size_t i) const { return !dim_cats[i].empty(); }

    void finalize_layout() {
        // strides: row-major, last dim innermost (stride 1)
        strides.assign(dims.size(), 0);
        std::uint64_t s = 1;
        for (std::size_t i = dims.size(); i-- > 0;) { strides[i] = s; s *= std::uint64_t(dims[i].size); }
        n_rows = 1; for (auto& d : dims) n_rows *= std::uint64_t(d.size);
        // record offsets
        offsets.assign(values.size(), 0);
        std::uint32_t off = 0;
        for (std::size_t i = 0; i < values.size(); ++i) { offsets[i] = off; off += val_width(values[i].type); }
        row_bytes = off;
    }
};

static constexpr char kTypedMagic[8] = {'C','K','L','T','Y','P','1','\0'};

// Pick rows-per-shard: a multiple of the innermost run, with shard bytes <= cap.
inline std::uint64_t typed_rows_per_shard(const TypedSchema& s, std::uint64_t max_bytes) {
    const std::uint64_t inner = std::uint64_t(s.dims.back().size);
    const std::uint64_t rb    = s.row_bytes ? s.row_bytes : 1;
    const std::uint64_t max_rows = max_bytes / rb;
    if (max_rows < inner)
        throw std::runtime_error("cklut: one innermost run exceeds max shard bytes; raise the cap or put a smaller dimension last");
    return (max_rows / inner) * inner;
}

// ===========================================================================
// TypedReader — runtime (non-templated) reader for the general format.
// ===========================================================================
class TypedReader {
public:
    explicit TypedReader(const std::string& meta_path, bool warm = false) {
        std::ifstream f(meta_path, std::ios::binary);
        if (!f) throw std::runtime_error("cklut: open meta: " + meta_path);
        std::string buf((std::istreambuf_iterator<char>(f)), std::istreambuf_iterator<char>());
        const char* p = buf.data();
        const char* end = p + buf.size();
        if (buf.size() < 8 || std::memcmp(p, kTypedMagic, 8) != 0)
            throw std::runtime_error("cklut: bad typed meta magic (not a CKLTYP1 file): " + meta_path);
        p += 8;
        detail::get_u32(p);                              // version
        std::uint32_t n_dims = detail::get_u32(p);
        std::uint32_t n_vals = detail::get_u32(p);
        detail::get_u32(p);                              // reserved
        sch_.n_rows         = detail::get_u64(p);
        sch_.rows_per_shard = detail::get_u64(p);
        sch_.n_shards       = detail::get_u32(p);
        sch_.row_bytes      = detail::get_u32(p);
        sch_.base_name      = get_str(p);

        sch_.dim_names.resize(n_dims); sch_.dims.resize(n_dims); sch_.dim_cats.resize(n_dims);
        dict_.resize(n_dims);
        for (std::uint32_t i = 0; i < n_dims; ++i) {
            sch_.dim_names[i] = get_str(p);
            std::uint8_t is_str = std::uint8_t(*p++);
            sch_.dims[i].min  = (std::int64_t)detail::get_u64(p);
            sch_.dims[i].size = (std::int64_t)detail::get_u64(p);
            if (is_str) {
                std::uint32_t n = detail::get_u32(p);
                for (std::uint32_t j = 0; j < n; ++j) {
                    std::string s = get_str(p);
                    dict_[i].emplace(s, j);
                    sch_.dim_cats[i].push_back(std::move(s));
                }
            }
        }
        sch_.values.resize(n_vals);
        for (std::uint32_t i = 0; i < n_vals; ++i) {
            sch_.values[i].name = get_str(p);
            sch_.values[i].type = ValType(std::uint8_t(*p++));
            if (sch_.values[i].type == ValType::STR) {
                std::uint32_t n = detail::get_u32(p);
                for (std::uint32_t j = 0; j < n; ++j) sch_.values[i].categories.push_back(get_str(p));
            }
        }
        (void)end;
        sch_.finalize_layout();   // recompute strides + offsets (consistency)

        single_shard_ = (sch_.n_shards <= 1);
        fdiv_.init(sch_.rows_per_shard ? sch_.rows_per_shard : 1);

        const std::string dir = detail::dir_of(meta_path);
        shards_.reserve(sch_.n_shards); base_.reserve(sch_.n_shards);
        for (std::uint32_t i = 0; i < sch_.n_shards; ++i) {
            shards_.push_back(std::make_unique<detail::FileMapRO>(detail::shard_path(dir, sch_.base_name, i)));
            base_.push_back(static_cast<const unsigned char*>(shards_.back()->data()));
        }
        if (warm) prefetch(true);
    }

    const TypedSchema& schema() const { return sch_; }
    std::uint64_t rows() const { return sch_.n_rows; }
    std::uint32_t n_values() const { return (std::uint32_t)sch_.values.size(); }

    std::uint64_t prefetch(bool blocking = false) const {
        std::uint64_t s = 0;
        for (const auto& m : shards_) { m->willneed(); if (blocking) s += m->populate(); }
        return s;
    }
    void advise_random()     const { for (const auto& m : shards_) m->advise_random(); }
    void advise_sequential() const { for (const auto& m : shards_) m->advise_sequential(); }
    void advise_normal()     const { for (const auto& m : shards_) m->advise_normal(); }

    // Encode one categorical key (label -> dense code).
    std::uint64_t category_index(std::size_t dim, std::string_view s) const {
        auto it = dict_[dim].find(std::string(s));
        if (it == dict_[dim].end()) throw std::runtime_error("cklut: unknown category '" + std::string(s) + "'");
        return it->second;
    }

    // Direct-address index from keys (numbers or string labels), no bounds check.
    template <typename... K>
    std::uint64_t index(K&&... keys) const {
        std::uint64_t idx = 0; std::size_t d = 0;
        ( (idx += encode_one(d, std::forward<K>(keys)) * sch_.strides[d], ++d), ... );
        return idx;
    }

    // Pointer to the fixed-width record for a row index.
    const unsigned char* record(std::uint64_t idx) const {
        if (single_shard_) return base_[0] + idx * sch_.row_bytes;
        std::uint64_t sh = fdiv_.div(idx), loc = idx - sh * sch_.rows_per_shard;
        return base_[sh] + loc * sch_.row_bytes;
    }
    template <typename... K>
    const unsigned char* at(K&&... keys) const { return record(index(std::forward<K>(keys)...)); }

    // Typed scalar decoders (read value column v out of a record). Out-of-range
    // / NA-stored cells decode to the relevant R NA.
    double      get_f64(const unsigned char* rec, std::size_t v) const { double d; std::memcpy(&d, rec + sch_.offsets[v], 8); return d; }
    float       get_f32(const unsigned char* rec, std::size_t v) const { float f; std::memcpy(&f, rec + sch_.offsets[v], 4); return f; }
    std::int32_t get_i32(const unsigned char* rec, std::size_t v) const { std::int32_t x; std::memcpy(&x, rec + sch_.offsets[v], 4); return x; }
    std::int64_t get_i64(const unsigned char* rec, std::size_t v) const { std::int64_t x; std::memcpy(&x, rec + sch_.offsets[v], 8); return x; }
    std::uint32_t get_code(const unsigned char* rec, std::size_t v) const { std::uint32_t x; std::memcpy(&x, rec + sch_.offsets[v], 4); return x; }

    // Decode any value column to a double (NA -> NaN). Handy for generic export.
    double get_as_double(const unsigned char* rec, std::size_t v) const {
        switch (sch_.values[v].type) {
            case ValType::F64: return get_f64(rec, v);
            case ValType::F32: { float f = get_f32(rec, v); return std::isnan(f) ? ck_na_f64() : double(f); }
            case ValType::I32: { auto x = get_i32(rec, v); return x == ck_na_i32 ? ck_na_f64() : double(x); }
            case ValType::I64: { auto x = get_i64(rec, v); return x == ck_na_i64 ? ck_na_f64() : double(x); }
            case ValType::LGL: { auto x = get_i32(rec, v); return x == ck_na_i32 ? ck_na_f64() : double(x); }
            case ValType::STR: return ck_na_f64();
        }
        return ck_na_f64();
    }
    // Decode a value column cell to its string label (for CSV export).
    std::string get_as_string(const unsigned char* rec, std::size_t v) const;

    // Decode a key dim's dense index back to its label/number string.
    std::string dim_label(std::size_t d, std::uint64_t idx) const {
        if (sch_.dim_is_string(d)) return sch_.dim_cats[d][idx];
        return std::to_string(sch_.dims[d].min + (std::int64_t)idx);
    }

private:
    static std::string get_str(const char*& p) { std::uint32_t n = detail::get_u32(p); std::string s(p, p + n); p += n; return s; }

    template <typename T>
    std::uint64_t encode_one(std::size_t d, const T& k) const {
        if constexpr (std::is_arithmetic_v<std::decay_t<T>>)
            return std::uint64_t(std::int64_t(k) - sch_.dims[d].min);
        else
            return category_index(d, std::string_view(k));
    }

    TypedSchema sch_;
    std::vector<std::unordered_map<std::string, std::uint32_t>> dict_;
    std::vector<std::unique_ptr<detail::FileMapRO>> shards_;
    std::vector<const unsigned char*> base_;
    detail::FastDivU64 fdiv_;
    bool single_shard_ = false;
};

inline std::string TypedReader::get_as_string(const unsigned char* rec, std::size_t v) const {
    const ValSpec& vs = sch_.values[v];
    switch (vs.type) {
        case ValType::F64: { double d = get_f64(rec, v); if (ck_is_na_f64(d)) return "NA"; char b[32]; std::snprintf(b, sizeof b, "%.17g", d); return b; }
        case ValType::F32: { float f = get_f32(rec, v); if (std::isnan(f)) return "NA"; char b[32]; std::snprintf(b, sizeof b, "%.9g", double(f)); return b; }
        case ValType::I32: { auto x = get_i32(rec, v); return x == ck_na_i32 ? std::string("NA") : std::to_string(x); }
        case ValType::I64: { auto x = get_i64(rec, v); return x == ck_na_i64 ? std::string("NA") : std::to_string(x); }
        case ValType::LGL: { auto x = get_i32(rec, v); return x == ck_na_i32 ? std::string("NA") : (x ? "TRUE" : "FALSE"); }
        case ValType::STR: { auto c = get_code(rec, v); return c == ck_na_str ? std::string("NA") : vs.categories[c]; }
    }
    return "NA";
}

// ===========================================================================
// TypedWriter — random-access fill of fixed-width records, sharded.
// ===========================================================================
class TypedWriter {
public:
    TypedWriter(const std::string& out_base,
                std::vector<std::string> dim_names,
                std::vector<DimSpec>     dim_specs,
                std::vector<ValSpec>     values,
                std::uint64_t max_bytes = 100ull * 1024 * 1024) {
        const std::string dir = detail::dir_of(out_base);
        sch_.base_name = dir.empty() ? out_base : out_base.substr(dir.size() + 1);
        sch_.dim_names = std::move(dim_names);
        sch_.values    = std::move(values);

        const std::size_t ND = dim_specs.size();
        sch_.dims.resize(ND); sch_.dim_cats.resize(ND); dict_.resize(ND);
        for (std::size_t i = 0; i < ND; ++i) {
            if (dim_specs[i].is_string()) {
                sch_.dims[i] = Dim{0, (std::int64_t)dim_specs[i].categories.size()};
                sch_.dim_cats[i] = dim_specs[i].categories;
                for (std::uint32_t j = 0; j < dim_specs[i].categories.size(); ++j)
                    dict_[i].emplace(dim_specs[i].categories[j], j);
            } else {
                sch_.dims[i] = Dim{dim_specs[i].min, dim_specs[i].size};
            }
        }
        // value string dictionaries -> code map
        vdict_.resize(sch_.values.size());
        for (std::size_t i = 0; i < sch_.values.size(); ++i)
            if (sch_.values[i].type == ValType::STR)
                for (std::uint32_t j = 0; j < sch_.values[i].categories.size(); ++j)
                    vdict_[i].emplace(sch_.values[i].categories[j], j);

        sch_.finalize_layout();
        sch_.rows_per_shard = typed_rows_per_shard(sch_, max_bytes);
        sch_.n_shards = std::uint32_t((sch_.n_rows + sch_.rows_per_shard - 1) / sch_.rows_per_shard);

        for (std::uint32_t i = 0; i < sch_.n_shards; ++i) {
            std::uint64_t rows = std::min<std::uint64_t>(sch_.rows_per_shard,
                                  sch_.n_rows - std::uint64_t(i) * sch_.rows_per_shard);
            maps_.push_back(std::make_unique<detail::FileMapRW>(
                detail::shard_path(dir, sch_.base_name, i), rows * sch_.row_bytes));
            base_.push_back(static_cast<unsigned char*>(maps_.back()->data()));
        }
        // Initialise every cell to NA so unfilled rows decode as NA, not zero.
        fill_na_all();
        write_manifest(out_base + ".ckmeta");
    }

    const TypedSchema& schema() const { return sch_; }
    std::uint64_t rows() const { return sch_.n_rows; }

    std::uint64_t index_cat(std::size_t dim, const std::string& s) const {
        auto it = dict_[dim].find(s);
        if (it == dict_[dim].end()) throw std::runtime_error("cklut: unknown category at build: '" + s + "'");
        return it->second;
    }
    std::uint64_t value_code(std::size_t v, const std::string& s) const {
        auto it = vdict_[v].find(s);
        if (it == vdict_[v].end()) throw std::runtime_error("cklut: unknown value category at build: '" + s + "'");
        return it->second;
    }

    unsigned char* record(std::uint64_t idx) {
        std::uint64_t sh = idx / sch_.rows_per_shard, loc = idx - sh * sch_.rows_per_shard;
        return base_[sh] + loc * sch_.row_bytes;
    }

    // Typed setters.
    void set_f64(unsigned char* rec, std::size_t v, double d)       { std::memcpy(rec + sch_.offsets[v], &d, 8); }
    void set_f32(unsigned char* rec, std::size_t v, float f)        { std::memcpy(rec + sch_.offsets[v], &f, 4); }
    void set_i32(unsigned char* rec, std::size_t v, std::int32_t x) { std::memcpy(rec + sch_.offsets[v], &x, 4); }
    void set_i64(unsigned char* rec, std::size_t v, std::int64_t x) { std::memcpy(rec + sch_.offsets[v], &x, 8); }
    void set_code(unsigned char* rec, std::size_t v, std::uint32_t c){ std::memcpy(rec + sch_.offsets[v], &c, 4); }

    // Parse a CSV/string cell into value column v (handles "NA"/"" per type).
    void set_from_string(unsigned char* rec, std::size_t v, const std::string& s) {
        const bool na = s.empty() || s == "NA" || s == "na" || s == "NaN";
        switch (sch_.values[v].type) {
            case ValType::F64: set_f64(rec, v, na ? ck_na_f64() : std::strtod(s.c_str(), nullptr)); break;
            case ValType::F32: set_f32(rec, v, na ? std::numeric_limits<float>::quiet_NaN() : std::strtof(s.c_str(), nullptr)); break;
            case ValType::I32: set_i32(rec, v, na ? ck_na_i32 : (std::int32_t)std::strtol(s.c_str(), nullptr, 10)); break;
            case ValType::I64: set_i64(rec, v, na ? ck_na_i64 : (std::int64_t)std::strtoll(s.c_str(), nullptr, 10)); break;
            case ValType::LGL: set_i32(rec, v, na ? ck_na_i32 : parse_bool(s)); break;
            case ValType::STR: set_code(rec, v, na ? ck_na_str : (std::uint32_t)value_code(v, s)); break;
        }
    }

private:
    static std::int32_t parse_bool(const std::string& s) {
        if (s == "TRUE"  || s == "true"  || s == "T" || s == "1") return 1;
        if (s == "FALSE" || s == "false" || s == "F" || s == "0") return 0;
        return ck_na_i32;
    }
    void fill_na_all() {
        // Write one NA record, then broadcast it across every row of every shard.
        std::vector<unsigned char> na(sch_.row_bytes);
        for (std::size_t v = 0; v < sch_.values.size(); ++v) {
            unsigned char* p = na.data();
            switch (sch_.values[v].type) {
                case ValType::F64: { double d = ck_na_f64(); std::memcpy(p + sch_.offsets[v], &d, 8); break; }
                case ValType::F32: { float f = std::numeric_limits<float>::quiet_NaN(); std::memcpy(p + sch_.offsets[v], &f, 4); break; }
                case ValType::I32: { std::int32_t x = ck_na_i32; std::memcpy(p + sch_.offsets[v], &x, 4); break; }
                case ValType::I64: { std::int64_t x = ck_na_i64; std::memcpy(p + sch_.offsets[v], &x, 8); break; }
                case ValType::LGL: { std::int32_t x = ck_na_i32; std::memcpy(p + sch_.offsets[v], &x, 4); break; }
                case ValType::STR: { std::uint32_t c = ck_na_str; std::memcpy(p + sch_.offsets[v], &c, 4); break; }
            }
        }
        for (std::uint32_t sh = 0; sh < sch_.n_shards; ++sh) {
            std::uint64_t rows = std::min<std::uint64_t>(sch_.rows_per_shard,
                                  sch_.n_rows - std::uint64_t(sh) * sch_.rows_per_shard);
            unsigned char* d = base_[sh];
            for (std::uint64_t r = 0; r < rows; ++r) std::memcpy(d + r * sch_.row_bytes, na.data(), sch_.row_bytes);
        }
    }
    static void put_str(std::vector<char>& b, const std::string& s) {
        detail::put_u32(b, (std::uint32_t)s.size()); b.insert(b.end(), s.begin(), s.end());
    }
    void write_manifest(const std::string& path) {
        std::vector<char> b; b.insert(b.end(), kTypedMagic, kTypedMagic + 8);
        detail::put_u32(b, 1);
        detail::put_u32(b, (std::uint32_t)sch_.dims.size());
        detail::put_u32(b, (std::uint32_t)sch_.values.size());
        detail::put_u32(b, 0);
        detail::put_u64(b, sch_.n_rows);
        detail::put_u64(b, sch_.rows_per_shard);
        detail::put_u32(b, sch_.n_shards);
        detail::put_u32(b, sch_.row_bytes);
        put_str(b, sch_.base_name);
        for (std::size_t i = 0; i < sch_.dims.size(); ++i) {
            put_str(b, sch_.dim_names[i]);
            bool is_str = !sch_.dim_cats[i].empty();
            b.push_back(is_str ? 1 : 0);
            detail::put_u64(b, (std::uint64_t)sch_.dims[i].min);
            detail::put_u64(b, (std::uint64_t)sch_.dims[i].size);
            if (is_str) { detail::put_u32(b, (std::uint32_t)sch_.dim_cats[i].size());
                          for (auto& s : sch_.dim_cats[i]) put_str(b, s); }
        }
        for (auto& vs : sch_.values) {
            put_str(b, vs.name);
            b.push_back((char)vs.type);
            if (vs.type == ValType::STR) { detail::put_u32(b, (std::uint32_t)vs.categories.size());
                                           for (auto& s : vs.categories) put_str(b, s); }
        }
        std::ofstream f(path, std::ios::binary | std::ios::trunc);
        if (!f) throw std::runtime_error("cklut: write meta: " + path);
        f.write(b.data(), (std::streamsize)b.size());
    }

    TypedSchema sch_;
    std::vector<std::unordered_map<std::string, std::uint32_t>> dict_, vdict_;
    std::vector<std::unique_ptr<detail::FileMapRW>> maps_;
    std::vector<unsigned char*> base_;
};

} // namespace cklut
