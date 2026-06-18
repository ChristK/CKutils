// test_typed.cpp — unit tests for the typed (mixed-type) cklut path.
//
//   build: g++ -std=c++17 -O2 -I.. test_typed.cpp -o test_typed
//   run:   ./test_typed
//
// Covers: every value type (f64/f32/i32/i64/lgl/str), NA round-trips, mixed
// integer + factor keys with non-zero min and non-alphabetical level order,
// CSV build -> read -> CSV export -> rebuild equality, and the multi-shard path.
#include "cklut_build_typed.hpp"
#include "cklut_typed.hpp"

#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <fstream>
#include <string>
#include <vector>

static int g_fail = 0, g_checks = 0;
#define CHECK(cond, msg) do { ++g_checks; if (!(cond)) { ++g_fail; \
    std::printf("FAIL: %s  (%s:%d)\n", msg, __FILE__, __LINE__); } } while (0)

static std::string tmp(const std::string& name) {
    const char* d = std::getenv("CKLUT_TMP"); std::string dir = d ? d : "/tmp";
    return dir + "/" + name;
}

// A tiny in-memory source so tests don't depend on file layout details.
struct MemSource : cklut::TypedRowSource {
    std::vector<std::string> cols;
    std::vector<std::vector<std::string>> rows;   // each row: all columns as strings
    std::vector<int> dcols, vcols; std::size_t pos = 0;
    std::vector<std::string> columns() override { return cols; }
    void select(std::vector<int> d, std::vector<int> v) override { dcols = std::move(d); vcols = std::move(v); }
    void restart() override { pos = 0; }
    bool next(std::vector<std::string>& dims, std::vector<std::string>& vals) override {
        if (pos >= rows.size()) return false;
        dims.resize(dcols.size()); vals.resize(vcols.size());
        for (std::size_t i = 0; i < dcols.size(); ++i) dims[i] = rows[pos][dcols[i]];
        for (std::size_t i = 0; i < vcols.size(); ++i) vals[i] = rows[pos][vcols[i]];
        ++pos; return true;
    }
};

// Build a dense table: keys (year:2018..2020, region:{north,south,east}),
// values of every supported type, deterministic functions of the keys, plus a
// couple of deliberately-NA cells.
static void make_rows(MemSource& src) {
    src.cols = {"year", "region", "vd", "vf", "vi", "vl", "vb", "vs"};
    const char* regions[] = {"north", "south", "east"};
    for (int year = 2018; year <= 2020; ++year)
        for (int rc = 0; rc < 3; ++rc) {
            std::vector<std::string> r(8);
            r[0] = std::to_string(year);
            r[1] = regions[rc];
            double d = year + rc / 10.0;
            r[2] = (year == 2019 && rc == 1) ? "NA" : std::to_string(d);            // vd: double (one NA)
            r[3] = std::to_string(float(rc) + 0.5f);                                 // vf: float
            r[4] = std::to_string(year * 10 + rc);                                   // vi: int
            r[5] = std::to_string(std::int64_t(year) * 1000000 + rc);                // vl: long
            r[6] = (rc % 2 == 0) ? "TRUE" : "FALSE";                                 // vb: logical
            r[7] = (year == 2020 && rc == 2) ? "NA" : (std::string("g") + regions[rc]); // vs: string (one NA)
            src.rows.push_back(std::move(r));
        }
}

static void test_build_read(std::uint64_t max_bytes, const char* tag) {
    MemSource src; make_rows(src);
    const std::string base = tmp(std::string("cktyp_") + tag);

    std::vector<cklut::TypedDim> dims = {
        { "year",   false, 0, 0, {} },                                  // int range (auto)
        { "region", true,  0, 0, { "north", "south", "east" } },        // factor levels (innermost)
    };
    std::vector<cklut::TypedVal> vals = {
        { "vd", cklut::ValType::F64, {} },
        { "vf", cklut::ValType::F32, {} },
        { "vi", cklut::ValType::I32, {} },
        { "vl", cklut::ValType::I64, {} },
        { "vb", cklut::ValType::LGL, {} },
        { "vs", cklut::ValType::STR, {} },
    };
    auto sch = cklut::build_lut_typed(src, base, dims, vals, max_bytes);
    CHECK(sch.n_rows == 9, "n_rows");

    cklut::TypedReader r(base + ".ckmeta");
    CHECK(r.rows() == 9, "reader rows");
    CHECK(r.n_values() == 6, "reader n_values");
    CHECK(r.schema().dim_names[0] == "year" && r.schema().dim_names[1] == "region", "dim names");
    CHECK(r.schema().values[0].name == "vd" && r.schema().values[5].name == "vs", "value names");

    const char* regions[] = {"north", "south", "east"};
    for (int year = 2018; year <= 2020; ++year)
        for (int rc = 0; rc < 3; ++rc) {
            const unsigned char* rec = r.at(year, regions[rc]);
            // vd (with one NA)
            double d = r.get_f64(rec, 0);
            if (year == 2019 && rc == 1) CHECK(cklut::ck_is_na_f64(d), "vd NA");
            else CHECK(std::fabs(d - (year + rc / 10.0)) < 1e-9, "vd value");
            // vf
            CHECK(std::fabs(r.get_f32(rec, 1) - (float(rc) + 0.5f)) < 1e-6, "vf value");
            // vi
            CHECK(r.get_i32(rec, 2) == year * 10 + rc, "vi value");
            // vl
            CHECK(r.get_i64(rec, 3) == std::int64_t(year) * 1000000 + rc, "vl value");
            // vb
            CHECK(r.get_i32(rec, 4) == (rc % 2 == 0 ? 1 : 0), "vb value");
            // vs (with one NA)
            std::uint32_t code = r.get_code(rec, 5);
            if (year == 2020 && rc == 2) CHECK(code == cklut::ck_na_str, "vs NA");
            else CHECK(r.schema().values[5].categories[code] == std::string("g") + regions[rc], "vs value");
        }
    std::printf("[%s] build/read: shards=%u rows/shard=%llu\n",
                tag, sch.n_shards, (unsigned long long)sch.rows_per_shard);
}

// Build -> export CSV -> rebuild -> compare records byte-for-byte.
static void test_csv_roundtrip() {
    MemSource src; make_rows(src);
    const std::string b1 = tmp("ckrt_a"), csv = tmp("ckrt.csv"), b2 = tmp("ckrt_b");
    std::vector<cklut::TypedDim> dims = {
        { "year", false, 0, 0, {} },
        { "region", true, 0, 0, { "north", "south", "east" } },
    };
    std::vector<cklut::TypedVal> vals = {
        { "vd", cklut::ValType::F64, {} }, { "vf", cklut::ValType::F32, {} },
        { "vi", cklut::ValType::I32, {} }, { "vl", cklut::ValType::I64, {} },
        { "vb", cklut::ValType::LGL, {} }, { "vs", cklut::ValType::STR, {} },
    };
    cklut::build_lut_typed(src, b1, dims, vals);
    { cklut::TypedReader r(b1 + ".ckmeta"); cklut::write_csv(r, csv); }

    // Rebuild from the exported CSV (types must be respecified; that's the API).
    cklut::CsvTypedRowSource csrc(csv, true, ',');
    cklut::build_lut_typed(csrc, b2, dims, vals);

    cklut::TypedReader r1(b1 + ".ckmeta"), r2(b2 + ".ckmeta");
    CHECK(r1.rows() == r2.rows(), "roundtrip rows");
    CHECK(r1.schema().row_bytes == r2.schema().row_bytes, "roundtrip row_bytes");
    bool eq = true;
    for (std::uint64_t i = 0; i < r1.rows(); ++i)
        if (std::memcmp(r1.record(i), r2.record(i), r1.schema().row_bytes) != 0) { eq = false; break; }
    CHECK(eq, "roundtrip records identical");
}

int main() {
    test_build_read(100ull * 1024 * 1024, "single");   // single shard
    test_build_read(96, "multi");                       // tiny cap -> one inner run/shard
    test_csv_roundtrip();

    std::printf("\n%d checks, %d failures\n", g_checks, g_fail);
    if (g_fail == 0) { std::puts("RESULT: PASS"); return 0; }
    std::puts("RESULT: FAIL"); return 1;
}
