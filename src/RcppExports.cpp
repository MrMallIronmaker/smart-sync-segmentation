// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// smart_segment_cpp
NumericVector smart_segment_cpp(NumericMatrix values, IntegerVector streak);
RcppExport SEXP _smartsyncseg_smart_segment_cpp(SEXP valuesSEXP, SEXP streakSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type values(valuesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type streak(streakSEXP);
    rcpp_result_gen = Rcpp::wrap(smart_segment_cpp(values, streak));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_smartsyncseg_smart_segment_cpp", (DL_FUNC) &_smartsyncseg_smart_segment_cpp, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_smartsyncseg(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
