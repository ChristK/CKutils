#!/usr/bin/env bash
# run_validation.sh — prove cklut reproduces CKutils::lookup_dt.
#
#   1. Rscript gen_expected.R  -> lookup_long.csv + queries_expected.csv
#      (oracle = bundled lookup_dt reference port; set USE_REAL_CKUTILS=1 to
#       use the real CKutils::lookup_dt instead).
#   2. compile validate_cklut.cpp against ../ (the cklut headers).
#   3. run it: builds a cklut table from lookup_long.csv and checks every
#      query row against lookup_dt's expected values.
#
# Exit status is non-zero if any row mismatches.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$here"

echo "== [1/3] generating data + expected results via lookup_dt oracle =="
Rscript gen_expected.R

echo "== [2/3] compiling cklut validator =="
CXX="${CXX:-g++}"
"$CXX" -std=c++17 -O2 -I.. validate_cklut.cpp -o validate_cklut

echo "== [3/3] validating cklut against lookup_dt =="
echo "-- single-shard table --"
./validate_cklut "$here"

echo "-- forced multi-shard table (CKLUT_MAX_BYTES=512) --"
CKLUT_MAX_BYTES=512 ./validate_cklut "$here"
