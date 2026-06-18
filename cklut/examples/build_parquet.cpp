// build_parquet — stream a Parquet file into a sharded .lut table.
// Requires Apache Arrow + Parquet (build with -DCKLUT_PARQUET=ON).
#include "cklut_parquet.hpp"
#include <iostream>

int main(int argc, char** argv) {
    if (argc < 3) { std::cerr << "usage: build_parquet <input.parquet> <out_base>\n"; return 1; }

    cklut::ParquetRowSource src(argv[1]);

    std::vector<cklut::BuildDim> dims = {
        { "sex",    true  },
        { "region", true  },
        { "age",    false },
    };
    std::vector<std::string> values = { "mean", "sd", "shape", "scale" };

    auto sch = cklut::build_lut(src, argv[2], dims, values, 100ull*1024*1024);
    std::cout << "rows=" << sch.n_rows << " shards=" << sch.n_shards << "\n";
    return 0;
}
