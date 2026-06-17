// bench_mt.cpp — OPTIONAL multi-threaded benchmark for cklut.
//
// The Reader is read-only after construction (const schema/strides/divider and
// a read-only memory map), so concurrent lookups need no locking. This measures
// how lookup/scan throughput scales across threads.
//
//   build: g++ -std=c++17 -O3 -march=native -DNDEBUG -pthread -I.. bench_mt.cpp -o bench_mt
//   run:   ./bench_mt [d0 d1 d2] [max_threads]
//          (defaults: 1000 1000 100 = 100,000,000 rows; max_threads = HW concurrency)

#include "cklut.hpp"
#include <chrono>
#include <cstdio>
#include <cstdint>
#include <random>
#include <thread>
#include <vector>
#include <string>
#include <algorithm>

using clk = std::chrono::steady_clock;
static double secs(clk::time_point a, clk::time_point b) {
    return std::chrono::duration<double>(b - a).count();
}

int main(int argc, char** argv) {
    const std::int64_t d0 = argc > 1 ? std::atoll(argv[1]) : 1000;
    const std::int64_t d1 = argc > 2 ? std::atoll(argv[2]) : 1000;
    const std::int64_t d2 = argc > 3 ? std::atoll(argv[3]) : 100;     // innermost
    unsigned maxT = argc > 4 ? (unsigned)std::atoi(argv[4])
                             : std::max(1u, std::thread::hardware_concurrency());
    const std::uint64_t n_rows = std::uint64_t(d0) * d1 * d2;
    const std::string base = "/tmp/ckbench_mt";
    constexpr std::size_t NV = 4;

    std::printf("table: %lld x %lld x %lld = %llu rows (%.2f GB), max_threads=%u\n",
                (long long)d0, (long long)d1, (long long)d2,
                (unsigned long long)n_rows, double(n_rows) * NV * 8 / 1e9, maxT);

    // ---- build once -----------------------------------------------------
    {
        std::vector<cklut::DimSpec> specs(3);
        specs[0].size = d0; specs[1].size = d1; specs[2].size = d2;
        cklut::ShardedWriter w(base, specs, NV, 100ull * 1024 * 1024);
        for (std::uint64_t i = 0; i < n_rows; ++i) {
            double* r = w.row(i);
            r[0] = double(i); r[1] = double(i)*2; r[2] = double(i)*3; r[3] = double(i)*4;
        }
        std::printf("built: shards=%u\n", w.schema().n_shards);
    }
    cklut::Reader<NV> lut(base + ".ckmeta");

    // ---- shared query sets ----------------------------------------------
    const std::size_t Q = 20'000'000;
    std::vector<std::int64_t> k0(Q), k1(Q), k2(Q);
    {
        std::mt19937_64 rng(123);
        for (std::size_t i = 0; i < Q; ++i) { k0[i]=rng()%d0; k1[i]=rng()%d1; k2[i]=rng()%d2; }
    }
    const std::size_t OUTER = 4'000'000;
    std::vector<std::int64_t> o0(OUTER), o1(OUTER);
    {
        std::mt19937_64 rng(99);
        for (std::size_t i = 0; i < OUTER; ++i) { o0[i]=rng()%d0; o1[i]=rng()%d1; }
    }

    auto run = [&](const char* name, unsigned T, auto work) {
        std::vector<double> partial(T, 0.0);
        auto t0 = clk::now();
        std::vector<std::thread> th;
        for (unsigned t = 0; t < T; ++t)
            th.emplace_back([&, t]{ partial[t] = work(t, T); });
        for (auto& x : th) x.join();
        double t = secs(t0, clk::now());
        double sink = 0; for (double v : partial) sink += v;
        return std::pair<double,double>(t, sink);
    };

    // ---- A. random lookups, scaling -------------------------------------
    std::puts("\nA. random lookups (DRAM-latency bound)");
    std::puts("  threads   ns/lookup   M lookups/s   speedup");
    double base_thru = 0;
    for (unsigned T = 1; T <= maxT; T <<= 1) {
        auto [t, sink] = run("rand", T, [&](unsigned tid, unsigned nT) {
            std::size_t lo = Q * tid / nT, hi = Q * (tid + 1) / nT;
            double acc = 0;
            for (std::size_t i = lo; i < hi; ++i) acc += lut.at(k0[i], k1[i], k2[i])[0];
            return acc;
        });
        double thru = Q / t / 1e6;
        if (T == 1) base_thru = thru;
        std::printf("  %5u     %8.2f    %9.1f    %6.2fx   [sink=%.0f]\n",
                    T, t / Q * 1e9, thru, thru / base_thru, sink);
        if (T == maxT) break;
    }

    // ---- B. hot lookups, scaling ----------------------------------------
    std::puts("\nB. hot lookups (cache-resident, arithmetic bound)");
    std::puts("  threads   ns/lookup   M lookups/s   speedup");
    const std::size_t HQ = 80'000'000;
    std::vector<std::int64_t> h0(4096), h1(4096), h2(4096);
    { std::mt19937_64 rng(7); const std::int64_t W = 64;
      for (std::size_t i=0;i<4096;++i){ h0[i]=rng()%W; h1[i]=rng()%W; h2[i]=rng()%std::min<std::int64_t>(W,d2);} }
    double hbase = 0;
    for (unsigned T = 1; T <= maxT; T <<= 1) {
        auto [t, sink] = run("hot", T, [&](unsigned tid, unsigned nT) {
            std::size_t lo = HQ * tid / nT, hi = HQ * (tid + 1) / nT;
            double acc = 0;
            for (std::size_t i = lo; i < hi; ++i) { std::size_t j=i&4095; acc += lut.at(h0[j],h1[j],h2[j])[0]; }
            return acc;
        });
        double thru = HQ / t / 1e6;
        if (T == 1) hbase = thru;
        std::printf("  %5u     %8.2f    %9.1f    %6.2fx   [sink=%.0f]\n",
                    T, t / HQ * 1e9, thru, thru / hbase, sink);
        if (T == maxT) break;
    }

    // ---- C. sequential scan, scaling ------------------------------------
    std::puts("\nC. sequential scan (bandwidth bound)");
    std::puts("  threads   ns/cell   M cells/s   GB/s    speedup");
    double sbase = 0;
    const double cells = double(OUTER) * d2;
    for (unsigned T = 1; T <= maxT; T <<= 1) {
        auto [t, sink] = run("scan", T, [&](unsigned tid, unsigned nT) {
            std::size_t lo = OUTER * tid / nT, hi = OUTER * (tid + 1) / nT;
            double acc = 0;
            for (std::size_t i = lo; i < hi; ++i) {
                auto s = lut.scan_inner(o0[i], o1[i]);
                for (std::size_t j = 0; j < s.count; ++j) acc += s.ptr[j * s.stride];
            }
            return acc;
        });
        double thru = cells / t / 1e6, gbs = cells * NV * 8 / t / 1e9;
        if (T == 1) sbase = thru;
        std::printf("  %5u    %7.2f   %8.1f   %5.2f   %6.2fx   [sink=%.0f]\n",
                    T, t / cells * 1e9, thru, gbs, thru / sbase, sink);
        if (T == maxT) break;
    }

    std::remove((base + ".ckmeta").c_str());
    for (unsigned i = 0; i < lut.schema().n_shards; ++i)
        std::remove(cklut::detail::shard_path("/tmp", "ckbench_mt", i).c_str());
    return 0;
}
