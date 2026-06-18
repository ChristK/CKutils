// build_csv — stream a CSV into a sharded .lut table.
// Adjust the dim/value column names to match your CSV header.
#include "cklut_build.hpp"
#include <iostream>

int main(int argc, char** argv) {
    if (argc < 3) { std::cerr << "usage: build_csv <input.csv> <out_base>\n"; return 1; }

    cklut::CsvRowSource src(argv[1], /*has_header=*/true, /*delim=*/',');

    // Key columns in dim order; put the dimension you SCAN over LAST.
    std::vector<cklut::BuildDim> dims = {
        { "sex",    /*is_string=*/true  },   // categories auto-discovered
        { "region", /*is_string=*/true  },
        { "age",    /*is_string=*/false },   // integer range auto-discovered (e.g. 30..99)
    };
    std::vector<std::string> values = { "mean", "sd", "shape", "scale" };

    auto sch = cklut::build_lut(src, argv[2], dims, values, /*max_bytes=*/100ull*1024*1024);
    std::cout << "rows=" << sch.n_rows
              << " shards=" << sch.n_shards
              << " rows/shard=" << sch.rows_per_shard << "\n";
    return 0;
}
