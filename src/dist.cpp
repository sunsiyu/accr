#include <Rcpp.h>
#include <actbase.h>
#include "dist.h"
using namespace Rcpp;

// Enable C++11
// [[Rcpp::plugins(cpp11)]]

//' Function to calculate euclidean distance between two vectors
//'
//' @param ptr1 Rcpp::XPtr of recording object 1
//' @param ptr2 Rcpp::Xptr of recording object 2
//' @param index1 start index for ptr1 (R notation)
//' @param index2 start index for ptr2 (R notation)
//' @param length total length to calculate
//'
//' @return A numeric vector of length one
//' @export
// [[Rcpp::export]]
double dist(SEXP ptr1, SEXP ptr2, uint64_t index1, uint64_t index2, uint64_t length) {

  Rcpp::NumericMatrix m1, m2;

  uint64_t n = length / CHUNKSIZE + 1;
  int16_t x1[3], x2[3];
  uint64_t dist1 = 0, dist2 = 0, dist3 = 0;
  for (uint32_t i = 1; i < n; i += CHUNKSIZE) {
    // either read chunksize or remaining space
    uint64_t toRead = min(CHUNKSIZE, length - i);

    m1 = actbase::ab_readAt(ptr1, i, toRead);
    m2 = actbase::ab_readAt(ptr2, i, toRead);

    for (uint64_t j = 0; j < toRead; j++) {
      x1[0] = m1(i, 0);
      x1[1] = m1(i, 1);
      x1[2] = m1(i, 2);
      x2[0] = m1(i, 0);
      x2[1] = m1(i, 1);
      x2[2] = m1(i, 2);

      dist1 += (x1[0] - x2[0])*(x1[0] - x2[0]);
      dist2 += (x1[1] - x2[1])*(x1[1] - x2[1]);
      dist3 += (x1[2] - x2[2])*(x1[2] - x2[2]);
    }
  }
  Rcpp::DataFrame out = Rcpp::DataFrame::create(Rcpp::Named("dist1") = dist1,
                                                Rcpp::Named("dist2") = dist2,
                                                Rcpp::Named("dist3") = dist3);
  return(out);
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
timesTwo(42)
*/
