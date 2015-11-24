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
Rcpp::List dist_cpp(SEXP ptr1, SEXP ptr2, uint64_t index1, uint64_t index2, uint64_t length) {

  Rcpp::NumericMatrix m1, m2;

  uint64_t n = length / CHUNKSIZE + 1;
  int16_t x1[3], x2[3];
  double dist1 = 0.0, dist2 = 0.0, dist3 = 0.0;
  uint64_t toRead;
  for (uint32_t i = 1; i <= n; i += CHUNKSIZE) {
    // either read chunksize or remaining space
    if (CHUNKSIZE > (length - (i-1)*CHUNKSIZE))
      toRead = length - (i-1)*CHUNKSIZE;
    else
      toRead = CHUNKSIZE;

    m1 = actbase::ab_readAt(ptr1, i, toRead);
    m2 = actbase::ab_readAt(ptr2, i, toRead);

    for (uint64_t j = 0; j < toRead; j++) {
      x1[0] = m1(j, 0);
      x1[1] = m1(j, 1);
      x1[2] = m1(j, 2);
      x2[0] = m2(j, 0);
      x2[1] = m2(j, 1);
      x2[2] = m2(j, 2);

      dist1 +=  (x1[0] - x2[0])*(x1[0] - x2[0]);
      dist2 +=  (x1[1] - x2[1])*(x1[1] - x2[1]);
      dist3 +=  (x1[2] - x2[2])*(x1[2] - x2[2]);
    }
  }
  Rcpp::List out = Rcpp::List::create(_["x"] = sqrt (dist1),
                                      _["y"] = sqrt (dist2),
                                      _["z"] = sqrt (dist3));
  return(out);
}

