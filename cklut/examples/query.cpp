// query — open a sharded .lut and do lookups / a range scan.
#include "cklut.hpp"
#include <iostream>

int main(int argc, char** argv) {
    if (argc < 2) { std::cerr << "usage: query <out_base>.ckmeta\n"; return 1; }

    cklut::Reader<4> lut(argv[1]);   // 4 = doubles per row (mean, sd, shape, scale)

    // Single lookup (mixed string / int keys).
    const double* p = lut.at("female", "south", 57);
    std::cout << "mean=" << p[0] << " sd=" << p[1] << "\n";

    // Fast path: pre-encode categoricals once, then loop with ints.
    std::uint64_t sex_i = lut.category_index(0, "female");
    std::uint64_t reg_i = lut.category_index(1, "south");

    // Contiguous range scan over the innermost dim (ages).
    auto s = lut.scan_inner(sex_i, reg_i);
    double sum = 0;
    for (std::size_t i = 0; i < s.count; ++i) sum += s.ptr[i * s.stride];   // sum of 'mean' over inner dim
    std::cout << "sum(mean) over inner dim = " << sum << "\n";
    return 0;
}
