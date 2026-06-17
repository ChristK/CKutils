// prefetch — demonstrate cklut's optional eager loading / page-cache warming.
//
// By default cklut is lazy: only the ~4 KB pages containing the rows you touch
// are paged in (the table can be larger than RAM). These options instead pull
// the whole table into RAM up front, which removes first-access page-fault
// latency for steady-state, latency-sensitive services that have the RAM.
//
//   build: g++ -std=c++17 -O3 -I.. prefetch.cpp -o prefetch
//   run:   ./prefetch <base>.ckmeta        (expects a 4-value table)

#include "cklut.hpp"
#include <chrono>
#include <iostream>
#include <string>

int main(int argc, char** argv) {
    if (argc < 2) { std::cerr << "usage: prefetch <base>.ckmeta\n"; return 1; }
    const std::string meta = argv[1];
    using clk = std::chrono::steady_clock;
    auto ms = [](clk::time_point a, clk::time_point b) {
        return std::chrono::duration<double, std::milli>(b - a).count();
    };

    // 1) Lazy (default): nothing in the payload is read until you touch a row.
    cklut::Reader<4> lazy(meta);
    std::cout << "lazy reader opened (" << lazy.rows()
              << " rows) — pages load on first touch\n";

    // 2) Async hint: kick off background readahead, return immediately.
    cklut::Reader<4> hinted(meta);
    hinted.prefetch();                       // non-blocking
    std::cout << "issued async readahead hint (non-blocking)\n";

    // 3) Blocking warm on an already-open reader: fully resident on return.
    cklut::Reader<4> r(meta);
    auto t0 = clk::now();
    r.prefetch(/*blocking=*/true);
    auto t1 = clk::now();
    std::cout << "blocking prefetch: whole table resident in " << ms(t0, t1) << " ms\n";

    // 4) Eager warm at construction (one-liner, equivalent to #3).
    auto t2 = clk::now();
    cklut::Reader<4> warm(meta, /*warm=*/true);
    auto t3 = clk::now();
    std::cout << "constructed warm reader in " << ms(t2, t3)
              << " ms (whole table in RAM before first lookup)\n";

    // From here, lookups on `warm`/`r` never page-fault on cold rows.

    // 5) The OPPOSITE case: a table LARGER THAN RAM with sparse/random access.
    //    Don't warm it (it won't fit) — instead disable readahead so each lookup
    //    faults in only the single page it needs and the resident set stays
    //    proportional to what you actually touch, not the whole file.
    cklut::Reader<4> sparse(meta);
    sparse.advise_random();
    std::cout << "opened a reader with MADV_RANDOM (larger-than-RAM sparse access)\n";

    return 0;
}
