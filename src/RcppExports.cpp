// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// dist
double dist(SEXP ptr1, SEXP ptr2, uint64_t index1, uint64_t index2, uint64_t length);
RcppExport SEXP accr_dist(SEXP ptr1SEXP, SEXP ptr2SEXP, SEXP index1SEXP, SEXP index2SEXP, SEXP lengthSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< SEXP >::type ptr1(ptr1SEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptr2(ptr2SEXP);
    Rcpp::traits::input_parameter< uint64_t >::type index1(index1SEXP);
    Rcpp::traits::input_parameter< uint64_t >::type index2(index2SEXP);
    Rcpp::traits::input_parameter< uint64_t >::type length(lengthSEXP);
    __result = Rcpp::wrap(dist(ptr1, ptr2, index1, index2, length));
    return __result;
END_RCPP
}