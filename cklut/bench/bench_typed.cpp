// bench_typed.cpp — show the typed (mixed-type) format costs nothing on the
// all-double case, and measure mixed-type record gather.
//
//   build: g++ -std=c++17 -O3 -march=native -DNDEBUG -I.. bench_typed.cpp -o bench_typed
//   run:   ./bench_typed [d0 d1 d2]   (default 1000 1000 100 = 100M rows)
//
// Regime A compares the legacy numeric Reader<4> against TypedReader on the
// SAME logical table (4 doubles/row, byte-identical payload). Regime B times a
// mixed-type record (f64,i32,lgl,str) gather.
#include "cklut.hpp"
#include "cklut_typed.hpp"
#include <chrono>
#include <cstdio>
#include <cstdint>
#include <random>
#include <vector>
#include <string>

using clk = std::chrono::steady_clock;
static double secs(clk::time_point a, clk::time_point b) {
    return std::chrono::duration<double>(b - a).count();
}

int main(int argc, char** argv) {
    const std::int64_t d0 = argc > 1 ? std::atoll(argv[1]) : 1000;
    const std::int64_t d1 = argc > 2 ? std::atoll(argv[2]) : 1000;
    const std::int64_t d2 = argc > 3 ? std::atoll(argv[3]) : 100;
    const std::uint64_t n_rows = std::uint64_t(d0) * d1 * d2;
    std::printf("table: %lld x %lld x %lld = %llu rows\n",
                (long long)d0, (long long)d1, (long long)d2, (unsigned long long)n_rows);

    // ---- build an all-f64 (4 values) typed table ------------------------------
    const std::string base = "/tmp/cktbench_f64";
    {
        std::vector<std::string> dn = {"a","b","c"};
        std::vector<cklut::DimSpec> ds(3);
        ds[0].size = d0; ds[1].size = d1; ds[2].size = d2;
        std::vector<cklut::ValSpec> vs = {
            {"v0",cklut::ValType::F64,{}},{"v1",cklut::ValType::F64,{}},
            {"v2",cklut::ValType::F64,{}},{"v3",cklut::ValType::F64,{}}};
        auto t0 = clk::now();
        cklut::TypedWriter w(base, dn, ds, vs, 100ull*1024*1024);
        for (std::uint64_t i = 0; i < n_rows; ++i) {
            unsigned char* r = w.record(i);
            for (int k = 0; k < 4; ++k) w.set_f64(r, k, double(i) * (k + 1));
        }
        auto t1 = clk::now();
        std::printf("build (typed f64): %.2f s (%.0f Mrows/s), shards=%u\n",
                    secs(t0,t1), n_rows/secs(t0,t1)/1e6, w.schema().n_shards);
    }

    // random keys
    const std::size_t Q = 10'000'000;
    std::mt19937_64 rng(123);
    std::vector<std::int64_t> k0(Q), k1(Q), k2(Q);
    for (std::size_t i = 0; i < Q; ++i) { k0[i]=rng()%d0; k1[i]=rng()%d1; k2[i]=rng()%d2; }

    // ---- A1. legacy numeric Reader<4> ----------------------------------------
    // (Reads the f64 payload directly; the typed manifest is a different magic,
    //  so build a parallel numeric table with the same contents for a fair A1.)
    {
        const std::string nb = "/tmp/cktbench_num";
        std::vector<cklut::DimSpec> ds(3); ds[0].size=d0; ds[1].size=d1; ds[2].size=d2;
        cklut::ShardedWriter w(nb, ds, 4, 100ull*1024*1024);
        for (std::uint64_t i = 0; i < n_rows; ++i) { double* r=w.row(i); for(int k=0;k<4;++k) r[k]=double(i)*(k+1); }
        cklut::Reader<4> lut(nb + ".ckmeta");
        auto t0 = clk::now(); double acc=0;
        for (std::size_t i=0;i<Q;++i){ const double* r=lut.at(k0[i],k1[i],k2[i]); acc+=r[0]+r[3]; }
        auto t1 = clk::now(); double t=secs(t0,t1);
        std::printf("A1 legacy Reader<4> : %.2f ns/lookup (%.1f M/s) [acc=%.0f]\n", t/Q*1e9, Q/t/1e6, acc);
    }

    // ---- A2. TypedReader, all-f64 --------------------------------------------
    {
        cklut::TypedReader lut(base + ".ckmeta");
        auto t0 = clk::now(); double acc=0;
        for (std::size_t i=0;i<Q;++i){ const unsigned char* r=lut.at(k0[i],k1[i],k2[i]); acc+=lut.get_f64(r,0)+lut.get_f64(r,3); }
        auto t1 = clk::now(); double t=secs(t0,t1);
        std::printf("A2 TypedReader f64  : %.2f ns/lookup (%.1f M/s) [acc=%.0f]\n", t/Q*1e9, Q/t/1e6, acc);
    }

    // ---- B. mixed-type record (f64,i32,lgl,str) gather -----------------------
    {
        const std::string mb = "/tmp/cktbench_mix";
        std::vector<std::string> dn = {"a","b","c"};
        std::vector<cklut::DimSpec> ds(3); ds[0].size=d0; ds[1].size=d1; ds[2].size=d2;
        std::vector<cklut::ValSpec> vs = {
            {"d",cklut::ValType::F64,{}},{"i",cklut::ValType::I32,{}},
            {"b",cklut::ValType::LGL,{}},{"s",cklut::ValType::STR,{"x","y","z"}}};
        cklut::TypedWriter w(mb, dn, ds, vs, 100ull*1024*1024);
        for (std::uint64_t i=0;i<n_rows;++i){ unsigned char* r=w.record(i);
            w.set_f64(r,0,double(i)); w.set_i32(r,1,(std::int32_t)i); w.set_i32(r,2,i&1); w.set_code(r,3,i%3); }
        cklut::TypedReader lut(mb + ".ckmeta");
        auto t0 = clk::now(); double acc=0;
        for (std::size_t i=0;i<Q;++i){ const unsigned char* r=lut.at(k0[i],k1[i],k2[i]);
            acc += lut.get_f64(r,0) + lut.get_i32(r,1) + lut.get_i32(r,2) + lut.get_code(r,3); }
        auto t1 = clk::now(); double t=secs(t0,t1);
        std::printf("B  TypedReader mixed: %.2f ns/lookup (%.1f M/s) [acc=%.0f]\n", t/Q*1e9, Q/t/1e6, acc);
    }
    return 0;
}
