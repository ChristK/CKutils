// bench.cpp — measure cklut lookup/scan efficiency on a large table.
//
//   build:  g++ -std=c++17 -O3 -march=native -DNDEBUG -I.. bench.cpp -o bench
//   run:    ./bench [d0 d1 d2]      (default 1000 1000 100 = 100,000,000 rows)
//
// Reports three regimes:
//   A. random lookups   — uniformly random keys over the whole table (DRAM bound)
//   B. hot lookups      — keys confined to a small window (index-arithmetic bound)
//   C. sequential scan  — scan_inner over the innermost dimension (bandwidth)

#include "cklut.hpp"
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
    const std::int64_t d2 = argc > 3 ? std::atoll(argv[3]) : 100;   // innermost / scan dim
    const std::uint64_t n_rows = std::uint64_t(d0) * d1 * d2;
    const std::string base = "/tmp/ckbench";
    constexpr std::size_t NV = 4;

    std::printf("table: %lld x %lld x %lld = %llu rows, %zu doubles/row (%.2f GB)\n",
                (long long)d0, (long long)d1, (long long)d2,
                (unsigned long long)n_rows, NV,
                double(n_rows) * NV * 8 / 1e9);

    // ---- build ----------------------------------------------------------
    {
        auto t0 = clk::now();
        std::vector<cklut::DimSpec> specs(3);
        specs[0].size = d0; specs[1].size = d1; specs[2].size = d2;
        cklut::ShardedWriter w(base, specs, NV, /*max_bytes=*/100ull * 1024 * 1024);
        for (std::uint64_t i = 0; i < n_rows; ++i) {
            double* r = w.row(i);
            r[0] = double(i); r[1] = double(i) * 2; r[2] = double(i) * 3; r[3] = double(i) * 4;
        }
        auto t1 = clk::now();
        std::printf("build: %.2f s (%.0f Mrows/s), shards=%u rows/shard=%llu\n",
                    secs(t0, t1), n_rows / secs(t0, t1) / 1e6,
                    w.schema().n_shards, (unsigned long long)w.schema().rows_per_shard);
    }

    cklut::Reader<NV> lut(base + ".ckmeta");

    // ---- A. random lookups ----------------------------------------------
    {
        const std::size_t Q = 10'000'000;
        std::mt19937_64 rng(123);
        std::vector<std::int64_t> k0(Q), k1(Q), k2(Q);
        for (std::size_t i = 0; i < Q; ++i) {
            k0[i] = rng() % d0; k1[i] = rng() % d1; k2[i] = rng() % d2;
        }
        auto t0 = clk::now();
        double acc = 0; std::uint64_t bad = 0;
        for (std::size_t i = 0; i < Q; ++i) {
            const double* r = lut.at(k0[i], k1[i], k2[i]);
            acc += r[0];
            std::uint64_t expect = (std::uint64_t(k0[i]) * d1 + k1[i]) * d2 + k2[i];
            if (double(expect) != r[0]) ++bad;
        }
        auto t1 = clk::now();
        double t = secs(t0, t1);
        std::printf("A. random  : %.2f ns/lookup  (%.1f M lookups/s)  [acc=%.0f bad=%llu]\n",
                    t / Q * 1e9, Q / t / 1e6, acc, (unsigned long long)bad);
    }

    // ---- B. hot lookups (small window, stays in cache) ------------------
    {
        const std::size_t Q = 50'000'000;
        const std::int64_t W = 64;                 // window of keys per dim
        std::mt19937_64 rng(7);
        std::vector<std::int64_t> k0(4096), k1(4096), k2(4096);
        for (std::size_t i = 0; i < k0.size(); ++i) {
            k0[i] = rng() % W; k1[i] = rng() % W; k2[i] = rng() % std::min<std::int64_t>(W, d2);
        }
        auto t0 = clk::now();
        double acc = 0;
        for (std::size_t i = 0; i < Q; ++i) {
            std::size_t j = i & 4095;
            acc += lut.at(k0[j], k1[j], k2[j])[0];
        }
        auto t1 = clk::now();
        double t = secs(t0, t1);
        std::printf("B. hot     : %.2f ns/lookup  (%.1f M lookups/s)  [acc=%.0f]\n",
                    t / Q * 1e9, Q / t / 1e6, acc);
    }

    // ---- C. sequential scan over innermost dim --------------------------
    {
        std::mt19937_64 rng(99);
        const std::size_t OUTER = 2'000'000;       // number of inner-runs scanned
        std::vector<std::int64_t> o0(OUTER), o1(OUTER);
        for (std::size_t i = 0; i < OUTER; ++i) { o0[i] = rng() % d0; o1[i] = rng() % d1; }
        auto t0 = clk::now();
        double acc = 0; std::uint64_t cells = 0;
        for (std::size_t i = 0; i < OUTER; ++i) {
            auto s = lut.scan_inner(o0[i], o1[i]);
            for (std::size_t j = 0; j < s.count; ++j) acc += s.ptr[j * s.stride];
            cells += s.count;
        }
        auto t1 = clk::now();
        double t = secs(t0, t1);
        std::printf("C. scan    : %.2f ns/cell  (%.1f M cells/s, %.2f GB/s read)  [acc=%.0f]\n",
                    t / cells * 1e9, cells / t / 1e6, double(cells) * NV * 8 / t / 1e9, acc);
    }
    return 0;
}
