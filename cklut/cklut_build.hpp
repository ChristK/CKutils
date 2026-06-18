// cklut_build.hpp — generic streaming builder. CSV source included.
#pragma once
#include "cklut.hpp"
#include <set>
#include <limits>
#include <algorithm>
#include <cstdlib>

namespace cklut {

// A streamable source of rows. The builder asks for the column list, tells the
// source which columns are dim-keys (as strings) and which are values (doubles),
// then iterates twice (pass 1 = discover, pass 2 = write).
struct RowSource {
    virtual ~RowSource() = default;
    virtual std::vector<std::string> columns() = 0;                       // available column names
    virtual void select(std::vector<int> dim_cols, std::vector<int> val_cols) = 0;
    virtual void restart() = 0;                                           // rewind to first row
    virtual bool next(std::vector<std::string>& dims, std::vector<double>& vals) = 0;
};

// How to treat each key column.
struct BuildDim {
    std::string column;             // column name (or decimal index if the source has no header)
    bool        is_string;          // true: categorical -> dictionary; false: integer range
    // Optional explicit spec; if left empty/zero it is auto-discovered in pass 1.
    std::int64_t min = 0, size = 0; // numeric: explicit range
    std::vector<std::string> categories;  // string: explicit ordering
};

// Build a sharded .lut from any RowSource. Returns the schema.
inline Schema build_lut(RowSource& src,
                        const std::string& out_base,
                        const std::vector<BuildDim>& bdims,
                        const std::vector<std::string>& value_columns,
                        std::uint64_t max_bytes = 100ull*1024*1024) {
    // resolve column indices
    auto cols = src.columns();
    auto find_col = [&](const std::string& name)->int{
        for (int i=0;i<(int)cols.size();++i) if (cols[i]==name) return i;
        char* end=nullptr; long v=std::strtol(name.c_str(),&end,10);
        if (end!=name.c_str() && *end=='\0' && v>=0 && v<(long)cols.size()) return (int)v;
        throw std::runtime_error("cklut: column not found: " + name);
    };
    std::vector<int> dcols, vcols;
    for (auto& d : bdims) dcols.push_back(find_col(d.column));
    for (auto& v : value_columns) vcols.push_back(find_col(v));
    src.select(dcols, vcols);

    const std::size_t ND = bdims.size();
    // ---- PASS 1: discovery (only for dims lacking an explicit spec) ----
    std::vector<bool> need(ND,false);
    std::vector<std::int64_t> nmin(ND, std::numeric_limits<std::int64_t>::max()),
                              nmax(ND, std::numeric_limits<std::int64_t>::min());
    std::vector<std::set<std::string>> sset(ND);
    for (std::size_t i=0;i<ND;++i)
        need[i] = bdims[i].is_string ? bdims[i].categories.empty() : (bdims[i].size==0);

    if (std::any_of(need.begin(),need.end(),[](bool b){return b;})) {
        src.restart();
        std::vector<std::string> dk; std::vector<double> vv;
        while (src.next(dk, vv)) {
            for (std::size_t i=0;i<ND;++i){
                if (!need[i]) continue;
                if (bdims[i].is_string) sset[i].insert(dk[i]);
                else { std::int64_t k = std::strtoll(dk[i].c_str(),nullptr,10);
                       nmin[i]=std::min(nmin[i],k); nmax[i]=std::max(nmax[i],k); }
            }
        }
    }

    // ---- finalize DimSpecs ----
    std::vector<DimSpec> specs(ND);
    for (std::size_t i=0;i<ND;++i){
        if (bdims[i].is_string){
            specs[i].categories = bdims[i].categories.empty()
                ? std::vector<std::string>(sset[i].begin(), sset[i].end())  // sorted, deterministic
                : bdims[i].categories;
        } else {
            specs[i].min  = need[i] ? nmin[i] : bdims[i].min;
            specs[i].size = need[i] ? (nmax[i]-nmin[i]+1) : bdims[i].size;
        }
    }

    // ---- PASS 2: write ----
    ShardedWriter w(out_base, specs, (std::uint32_t)value_columns.size(), max_bytes);
    src.restart();
    std::vector<std::string> dk; std::vector<double> vv;
    while (src.next(dk, vv)) {
        std::uint64_t idx = 0;
        for (std::size_t d=0; d<ND; ++d){
            std::uint64_t e = bdims[d].is_string ? w.index_cat(d, dk[d])
                                                 : std::uint64_t(std::strtoll(dk[d].c_str(),nullptr,10) - w.schema().dims[d].min);
            idx += e * w.schema().strides[d];
        }
        double* r = w.row(idx);
        for (std::uint32_t v=0; v<vv.size(); ++v) r[v] = vv[v];
    }
    return w.schema();
}

// ----------------------------- CSV source ----------------------------------
class CsvRowSource : public RowSource {
public:
    CsvRowSource(std::string path, bool has_header=true, char delim=',')
        : path_(std::move(path)), has_header_(has_header), delim_(delim) {}

    std::vector<std::string> columns() override {
        std::ifstream f(path_); if(!f) throw std::runtime_error("cklut: open csv: "+path_);
        std::string line; std::getline(f, line); strip_cr(line);
        std::vector<std::string> fields; split(line, fields);
        if (has_header_) return fields;
        std::vector<std::string> idx(fields.size());
        for (std::size_t i=0;i<fields.size();++i) idx[i]=std::to_string(i);
        return idx;
    }
    void select(std::vector<int> dim_cols, std::vector<int> val_cols) override {
        dcols_=std::move(dim_cols); vcols_=std::move(val_cols);
    }
    void restart() override {
        f_.close(); f_.clear(); f_.open(path_);
        if(!f_) throw std::runtime_error("cklut: reopen csv: "+path_);
        if (has_header_){ std::string h; std::getline(f_,h); }
    }
    bool next(std::vector<std::string>& dims, std::vector<double>& vals) override {
        std::string line;
        if (!std::getline(f_, line)) return false;
        strip_cr(line);
        if (line.empty()) return next(dims, vals);
        split(line, buf_);
        dims.resize(dcols_.size()); vals.resize(vcols_.size());
        for (std::size_t i=0;i<dcols_.size();++i) dims[i] = buf_.at(dcols_[i]);
        for (std::size_t i=0;i<vcols_.size();++i) vals[i] = std::strtod(buf_.at(vcols_[i]).c_str(), nullptr);
        return true;
    }
private:
    static void strip_cr(std::string& s){ if(!s.empty() && s.back()=='\r') s.pop_back(); }
    void split(const std::string& line, std::vector<std::string>& out){
        out.clear(); std::string cur; bool q=false;
        for(char c: line){
            if(c=='"') q=!q;
            else if(c==delim_ && !q){ out.push_back(cur); cur.clear(); }
            else cur.push_back(c);
        }
        out.push_back(cur);
    }
    std::string path_; bool has_header_; char delim_;
    std::ifstream f_;
    std::vector<int> dcols_, vcols_;
    std::vector<std::string> buf_;
};

} // namespace cklut
