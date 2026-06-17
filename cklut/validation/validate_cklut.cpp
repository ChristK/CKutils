// validate_cklut.cpp
// Builds a cklut table from lookup_long.csv (the exact dense lookup table that
// CKutils::lookup_dt consumes) and checks that, for every query row in
// queries_expected.csv, cklut returns the SAME values that lookup_dt produced.
//
// Key-order and factor-level handling are pinned to match lookup_dt:
//   * dim order = [year, age, region, sex]  (lookup_dt's `on`: sorted, year-first)
//   * factor keys use their LEVEL order as the category order (not alphabetical)
//   * integer keys index as (value - min)
//
// Exit status 0 = all rows match; 1 = mismatch (details printed).

#include "cklut_build.hpp"
#include "cklut.hpp"

#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

namespace {

void split(const std::string& line, char d, std::vector<std::string>& out) {
    out.clear();
    std::string cur;
    for (char c : line) {
        if (c == d) { out.push_back(cur); cur.clear(); }
        else if (c != '\r') cur.push_back(c);
    }
    out.push_back(cur);
}

int col(const std::vector<std::string>& hdr, const std::string& name) {
    for (int i = 0; i < (int)hdr.size(); ++i) if (hdr[i] == name) return i;
    std::cerr << "missing column: " << name << "\n"; std::exit(2);
}

} // namespace

int main(int argc, char** argv) {
    const std::string dir       = (argc > 1) ? argv[1] : ".";
    const std::string long_csv  = dir + "/lookup_long.csv";
    const std::string query_csv = dir + "/queries_expected.csv";
    const std::string out_base  = dir + "/cklut_table";

    // ---- build the cklut table from the same dense lookup table ----------
    cklut::CsvRowSource src(long_csv, /*has_header=*/true, ',');
    std::vector<cklut::BuildDim> dims = {
        { "year",   false, 2018, 3, {} },                                  // int range
        { "age",    false, 40,  60, {} },                                  // int range
        { "region", true,  0,   0,  { "north", "south", "east", "west" } },// factor levels
        { "sex",    true,  0,   0,  { "men", "women" } },                  // factor levels (innermost)
    };
    std::vector<std::string> values = { "mu", "sigma", "p" };
    // Allow forcing small shards (CKLUT_MAX_BYTES) to also exercise the
    // multi-shard path against lookup_dt's results.
    std::uint64_t max_bytes = 100ull * 1024 * 1024;
    if (const char* e = std::getenv("CKLUT_MAX_BYTES")) {
        std::uint64_t v = std::strtoull(e, nullptr, 10);
        if (v) max_bytes = v;
    }
    auto sch = cklut::build_lut(src, out_base, dims, values, max_bytes);
    std::cout << "built cklut: rows=" << sch.n_rows
              << " shards=" << sch.n_shards
              << " rows/shard=" << sch.rows_per_shard << "\n";

    cklut::Reader<3> lut(out_base + ".ckmeta");

    // ---- compare against lookup_dt's expected values ---------------------
    std::ifstream f(query_csv);
    if (!f) { std::cerr << "cannot open " << query_csv << "\n"; return 2; }
    std::string line;
    std::getline(f, line);
    std::vector<std::string> hdr; split(line, ',', hdr);
    const int cy = col(hdr, "year"), ca = col(hdr, "age"),
              cr = col(hdr, "region"), cs = col(hdr, "sex"),
              cmu = col(hdr, "mu"), csg = col(hdr, "sigma"), cp = col(hdr, "p");

    long n = 0, mism = 0;
    double max_abs = 0.0, max_rel = 0.0;
    std::vector<std::string> fld;
    while (std::getline(f, line)) {
        if (line.empty()) continue;
        split(line, ',', fld);
        const int    year = std::atoi(fld[cy].c_str());
        const int    age  = std::atoi(fld[ca].c_str());
        const std::string& region = fld[cr];
        const std::string& sex    = fld[cs];
        const double exp[3] = { std::strtod(fld[cmu].c_str(), nullptr),
                                std::strtod(fld[csg].c_str(), nullptr),
                                std::strtod(fld[cp].c_str(),  nullptr) };

        const double* got = lut.at(year, age, region, sex);

        bool row_bad = false;
        for (int k = 0; k < 3; ++k) {
            double a = std::fabs(got[k] - exp[k]);
            double r = a / (std::fabs(exp[k]) + 1e-300);
            if (a > max_abs) max_abs = a;
            if (r > max_rel) max_rel = r;
            if (a > 1e-9 && r > 1e-12) row_bad = true;
        }
        if (row_bad) {
            if (mism < 5)
                std::printf("MISMATCH row %ld  key=(%d,%d,%s,%s)  got=[%.17g,%.17g,%.17g] exp=[%.17g,%.17g,%.17g]\n",
                            n, year, age, region.c_str(), sex.c_str(),
                            got[0], got[1], got[2], exp[0], exp[1], exp[2]);
            ++mism;
        }
        ++n;
    }

    std::printf("checked %ld query rows | mismatches=%ld | max_abs=%.3g max_rel=%.3g\n",
                n, mism, max_abs, max_rel);
    if (mism == 0) { std::puts("RESULT: PASS — cklut matches lookup_dt on every row"); return 0; }
    std::puts("RESULT: FAIL");
    return 1;
}
