/* CKutils: an R package with some utility functions I use regularly
Copyright (C) 2025  Chris Kypridemos

CKutils is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, see <http://www.gnu.org/licenses/>
or write to the Free Software Foundation, Inc., 51 Franklin Street,
Fifth Floor, Boston, MA 02110-1301  USA. */

// Internal bridges to the header-only random-generation scalars.
//
// frNBI_scalar / frZANBI_scalar / frZINBI_scalar are inline kernels in
// inst/include/ for LinkingTo: CKutils consumers; nothing else in this package
// calls them, so without a bridge they would be unreachable from R and could
// not be tested. These three functions map a vector of uniforms through the
// corresponding kernel, which is exactly what the exported R frNBI() /
// frZANBI() / frZINBI() do with dqrng::dqrunif() -- so a test can feed both the
// same uniforms and assert identical output (inst/tinytest/test-rng_distr.R).
//
// They are deliberately NOT exported: no roxygen block, and the R-side names
// are dot-prefixed, so they stay internal (CKutils:::.frNBI_scalar_vec) and
// cannot shadow or be confused with the exported samplers.

#include <Rcpp.h>
#include "distr_NBI.h"
#include "distr_ZANBI.h"
#include "distr_ZINBI.h"
// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

// [[Rcpp::export(.frNBI_scalar_vec)]]
IntegerVector frNBI_scalar_vec(const NumericVector& u,
                               const double& mu,
                               const double& sigma)
{
  const int n = u.size();
  IntegerVector out(n);
  for (int i = 0; i < n; i++) out[i] = frNBI_scalar(u[i], mu, sigma);
  return out;
}

// [[Rcpp::export(.frZANBI_scalar_vec)]]
IntegerVector frZANBI_scalar_vec(const NumericVector& u,
                                 const double& mu,
                                 const double& sigma,
                                 const double& nu)
{
  const int n = u.size();
  IntegerVector out(n);
  for (int i = 0; i < n; i++) out[i] = frZANBI_scalar(u[i], mu, sigma, nu);
  return out;
}

// [[Rcpp::export(.frZINBI_scalar_vec)]]
IntegerVector frZINBI_scalar_vec(const NumericVector& u,
                                 const double& mu,
                                 const double& sigma,
                                 const double& nu)
{
  const int n = u.size();
  IntegerVector out(n);
  for (int i = 0; i < n; i++) out[i] = frZINBI_scalar(u[i], mu, sigma, nu);
  return out;
}
