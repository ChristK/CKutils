// cklut_build_typed.hpp — generic streaming builder for the typed (mixed-type)
// format, plus a dependency-free CSV source and a CSV exporter.
//
// Mirrors cklut_build.hpp but value cells are carried as raw strings and parsed
// according to each value column's declared ValType, so values may be double,
// float, int, long, logical or string/factor.
#pragma once
#include "cklut_typed.hpp"
#include <set>
#include <limits>
#include <algorithm>
#include <cstdlib>
#include <cstdio>

namespace cklut {

// A streamable source of rows for the typed builder: keys AND values as strings.
struct TypedRowSource {
    virtual ~TypedRowSource() = default;
    virtual std::vector<std::string> columns() = 0;
    virtual void select(std::vector<int> dim_cols, std::vector<int> val_cols) = 0;
    virtual void restart() = 0;
    virtual bool next(std::vector<std::string>& dims, std::vector<std::string>& vals) = 0;
};

// Key column description (same idea as BuildDim).
struct TypedDim {
    std::string column;
    bool        is_string = false;
    std::int64_t min = 0, size = 0;            // numeric: explicit range (else auto)
    std::vector<std::string> categories;       // string: explicit order (else auto, sorted)
};

// Value column description: name + type. STR categories auto-discovered if empty.
struct TypedVal {
    std::string column;
    ValType     type = ValType::F64;
    std::vector<std::string> categories;       // STR: explicit dictionary order (else auto)
};

// Build a typed sharded table from any TypedRowSource. Returns the schema.
inline TypedSchema build_lut_typed(TypedRowSource& src,
                                   const std::string& out_base,
                                   const std::vector<TypedDim>& bdims,
                                   const std::vector<TypedVal>& bvals,
                                   std::uint64_t max_bytes = 100ull * 1024 * 1024) {
    auto cols = src.columns();
    auto find_col = [&](const std::string& name) -> int {
        for (int i = 0; i < (int)cols.size(); ++i) if (cols[i] == name) return i;
        char* e = nullptr; long v = std::strtol(name.c_str(), &e, 10);
        if (e != name.c_str() && *e == '\0' && v >= 0 && v < (long)cols.size()) return (int)v;
        throw std::runtime_error("cklut: column not found: " + name);
    };
    std::vector<int> dcols, vcols;
    for (auto& d : bdims) dcols.push_back(find_col(d.column));
    for (auto& v : bvals) vcols.push_back(find_col(v.column));
    src.select(dcols, vcols);

    const std::size_t ND = bdims.size(), NVL = bvals.size();

    // ---- PASS 1: discover dim ranges/cats + STR-value dictionaries ----------
    std::vector<bool> need_dim(ND, false);
    std::vector<std::int64_t> nmin(ND, std::numeric_limits<std::int64_t>::max()),
                              nmax(ND, std::numeric_limits<std::int64_t>::min());
    std::vector<std::set<std::string>> dset(ND), vset(NVL);
    for (std::size_t i = 0; i < ND; ++i)
        need_dim[i] = bdims[i].is_string ? bdims[i].categories.empty() : (bdims[i].size == 0);
    std::vector<bool> need_val(NVL, false);
    for (std::size_t i = 0; i < NVL; ++i)
        need_val[i] = (bvals[i].type == ValType::STR) && bvals[i].categories.empty();

    bool any = std::any_of(need_dim.begin(), need_dim.end(), [](bool b){return b;}) ||
               std::any_of(need_val.begin(), need_val.end(), [](bool b){return b;});
    if (any) {
        src.restart();
        std::vector<std::string> dk, vv;
        while (src.next(dk, vv)) {
            for (std::size_t i = 0; i < ND; ++i) {
                if (!need_dim[i]) continue;
                if (bdims[i].is_string) dset[i].insert(dk[i]);
                else { std::int64_t k = std::strtoll(dk[i].c_str(), nullptr, 10);
                       nmin[i] = std::min(nmin[i], k); nmax[i] = std::max(nmax[i], k); }
            }
            for (std::size_t i = 0; i < NVL; ++i) {
                if (!need_val[i]) continue;
                const std::string& s = vv[i];
                if (!(s.empty() || s == "NA")) vset[i].insert(s);
            }
        }
    }

    // ---- finalize specs -----------------------------------------------------
    std::vector<std::string> dim_names(ND);
    std::vector<DimSpec> dspecs(ND);
    for (std::size_t i = 0; i < ND; ++i) {
        dim_names[i] = bdims[i].column;
        if (bdims[i].is_string) {
            dspecs[i].categories = bdims[i].categories.empty()
                ? std::vector<std::string>(dset[i].begin(), dset[i].end())
                : bdims[i].categories;
        } else {
            dspecs[i].min  = need_dim[i] ? nmin[i] : bdims[i].min;
            dspecs[i].size = need_dim[i] ? (nmax[i] - nmin[i] + 1) : bdims[i].size;
        }
    }
    std::vector<ValSpec> vspecs(NVL);
    for (std::size_t i = 0; i < NVL; ++i) {
        vspecs[i].name = bvals[i].column;
        vspecs[i].type = bvals[i].type;
        if (bvals[i].type == ValType::STR)
            vspecs[i].categories = bvals[i].categories.empty()
                ? std::vector<std::string>(vset[i].begin(), vset[i].end())
                : bvals[i].categories;
    }

    // ---- PASS 2: write ------------------------------------------------------
    TypedWriter w(out_base, dim_names, dspecs, vspecs, max_bytes);
    src.restart();
    std::vector<std::string> dk, vv;
    while (src.next(dk, vv)) {
        std::uint64_t idx = 0;
        for (std::size_t d = 0; d < ND; ++d) {
            std::uint64_t e = bdims[d].is_string
                ? w.index_cat(d, dk[d])
                : std::uint64_t(std::strtoll(dk[d].c_str(), nullptr, 10) - w.schema().dims[d].min);
            idx += e * w.schema().strides[d];
        }
        unsigned char* rec = w.record(idx);
        for (std::size_t v = 0; v < vv.size(); ++v) w.set_from_string(rec, v, vv[v]);
    }
    return w.schema();
}

// ----------------------------- CSV source ----------------------------------
class CsvTypedRowSource : public TypedRowSource {
public:
    CsvTypedRowSource(std::string path, bool has_header = true, char delim = ',')
        : path_(std::move(path)), has_header_(has_header), delim_(delim) {}

    std::vector<std::string> columns() override {
        std::ifstream f(path_); if (!f) throw std::runtime_error("cklut: open csv: " + path_);
        std::string line; std::getline(f, line); strip_cr(line);
        std::vector<std::string> fields; split(line, fields);
        if (has_header_) return fields;
        std::vector<std::string> idx(fields.size());
        for (std::size_t i = 0; i < fields.size(); ++i) idx[i] = std::to_string(i);
        return idx;
    }
    void select(std::vector<int> dim_cols, std::vector<int> val_cols) override {
        dcols_ = std::move(dim_cols); vcols_ = std::move(val_cols);
    }
    void restart() override {
        f_.close(); f_.clear(); f_.open(path_);
        if (!f_) throw std::runtime_error("cklut: reopen csv: " + path_);
        if (has_header_) { std::string h; std::getline(f_, h); }
    }
    bool next(std::vector<std::string>& dims, std::vector<std::string>& vals) override {
        std::string line;
        if (!std::getline(f_, line)) return false;
        strip_cr(line);
        if (line.empty()) return next(dims, vals);
        split(line, buf_);
        dims.resize(dcols_.size()); vals.resize(vcols_.size());
        for (std::size_t i = 0; i < dcols_.size(); ++i) dims[i] = buf_.at(dcols_[i]);
        for (std::size_t i = 0; i < vcols_.size(); ++i) vals[i] = buf_.at(vcols_[i]);
        return true;
    }
private:
    static void strip_cr(std::string& s) { if (!s.empty() && s.back() == '\r') s.pop_back(); }
    void split(const std::string& line, std::vector<std::string>& out) {
        out.clear(); std::string cur; bool q = false;
        for (char c : line) {
            if (c == '"') q = !q;
            else if (c == delim_ && !q) { out.push_back(cur); cur.clear(); }
            else cur.push_back(c);
        }
        out.push_back(cur);
    }
    std::string path_; bool has_header_; char delim_;
    std::ifstream f_;
    std::vector<int> dcols_, vcols_;
    std::vector<std::string> buf_;
};

// --------------------------- CSV export ------------------------------------
// Write the whole table out as long CSV (keys + values), row-major, so it can
// round-trip back through the builder. Factors/strings are written as labels and
// doubles at full (17-digit) precision. Mirrors data.table::fwrite output.
inline void write_csv(const TypedReader& r, const std::string& path, char delim = ',') {
    const TypedSchema& s = r.schema();
    std::ofstream f(path, std::ios::binary | std::ios::trunc);
    if (!f) throw std::runtime_error("cklut: write csv: " + path);
    std::string out;
    out.reserve(1 << 20);
    // header
    for (std::size_t i = 0; i < s.dim_names.size(); ++i) { if (i) out += delim; out += s.dim_names[i]; }
    for (auto& v : s.values) { out += delim; out += v.name; }
    out += '\n';

    const std::size_t ND = s.dims.size();
    std::vector<std::uint64_t> kidx(ND, 0);   // current per-dim dense index
    for (std::uint64_t row = 0; row < s.n_rows; ++row) {
        const unsigned char* rec = r.record(row);
        for (std::size_t d = 0; d < ND; ++d) { if (d) out += delim; out += r.dim_label(d, kidx[d]); }
        for (std::size_t v = 0; v < s.values.size(); ++v) { out += delim; out += r.get_as_string(rec, v); }
        out += '\n';
        if (out.size() > (1 << 20)) { f.write(out.data(), (std::streamsize)out.size()); out.clear(); }
        // increment mixed-radix key counter (last dim fastest)
        for (std::size_t d = ND; d-- > 0;) {
            if (++kidx[d] < (std::uint64_t)s.dims[d].size) break;
            kidx[d] = 0;
        }
    }
    if (!out.empty()) f.write(out.data(), (std::streamsize)out.size());
}

} // namespace cklut
