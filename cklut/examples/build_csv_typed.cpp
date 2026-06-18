// build_csv_typed.cpp — build a typed (mixed-type) cklut table from a CSV.
//
//   build_csv_typed <in.csv> <out_base> <key=type,...> <val=type,...>
//
// key types: int | str        (str = factor/dictionary)
// val types: f64|f32|i32|i64|lgl|str
//
// Example:
//   build_csv_typed dist.csv dist \
//     year=int,age=int,region=str,sex=str  mu=f64,cnt=i32,grp=str,flag=lgl
//
// Numeric key ranges and string dictionaries are auto-discovered. Put the
// dimension you scan over last.
#include "cklut_build_typed.hpp"
#include <cstdio>
#include <sstream>

static std::vector<std::pair<std::string,std::string>> parse_spec(const std::string& s) {
    std::vector<std::pair<std::string,std::string>> out;
    std::stringstream ss(s); std::string tok;
    while (std::getline(ss, tok, ',')) {
        auto eq = tok.find('=');
        if (eq == std::string::npos) { std::fprintf(stderr, "bad spec '%s'\n", tok.c_str()); std::exit(2); }
        out.emplace_back(tok.substr(0, eq), tok.substr(eq + 1));
    }
    return out;
}

int main(int argc, char** argv) {
    if (argc < 5) { std::fprintf(stderr,
        "usage: %s <in.csv> <out_base> <key=type,...> <val=type,...>\n", argv[0]); return 2; }
    const std::string in = argv[1], out = argv[2];
    auto ks = parse_spec(argv[3]);
    auto vsx = parse_spec(argv[4]);

    std::vector<cklut::TypedDim> dims;
    for (auto& k : ks) {
        cklut::TypedDim d; d.column = k.first;
        d.is_string = (k.second == "str" || k.second == "factor" || k.second == "string");
        dims.push_back(std::move(d));
    }
    std::vector<cklut::TypedVal> vals;
    for (auto& v : vsx) { cklut::TypedVal tv; tv.column = v.first; tv.type = cklut::val_type_from_name(v.second); vals.push_back(std::move(tv)); }

    cklut::CsvTypedRowSource src(in, /*has_header=*/true, ',');
    auto sch = cklut::build_lut_typed(src, out, dims, vals);
    std::printf("built %s: rows=%llu shards=%u row_bytes=%u\n",
                out.c_str(), (unsigned long long)sch.n_rows, sch.n_shards, sch.row_bytes);
    return 0;
}
