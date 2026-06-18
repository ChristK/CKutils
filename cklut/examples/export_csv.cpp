// export_csv.cpp — read a typed cklut table and write it back out as long CSV
// (keys + values), row-major. Round-trips through build_csv_typed.
//
//   export_csv <in_base.ckmeta> <out.csv>
#include "cklut_build_typed.hpp"
#include <cstdio>

int main(int argc, char** argv) {
    if (argc < 3) { std::fprintf(stderr, "usage: %s <in.ckmeta> <out.csv>\n", argv[0]); return 2; }
    cklut::TypedReader r(argv[1]);
    cklut::write_csv(r, argv[2]);
    std::printf("wrote %llu rows to %s\n", (unsigned long long)r.rows(), argv[2]);
    return 0;
}
