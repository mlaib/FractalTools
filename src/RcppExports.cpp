// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// BigArmaEuc
void BigArmaEuc(SEXP pInBigMat, SEXP pOutBigMat);
RcppExport SEXP _FractalTools_BigArmaEuc(SEXP pInBigMatSEXP, SEXP pOutBigMatSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type pInBigMat(pInBigMatSEXP);
    Rcpp::traits::input_parameter< SEXP >::type pOutBigMat(pOutBigMatSEXP);
    BigArmaEuc(pInBigMat, pOutBigMat);
    return R_NilValue;
END_RCPP
}
// CPP_get_openmp_threads
DataFrame CPP_get_openmp_threads();
RcppExport SEXP _FractalTools_CPP_get_openmp_threads() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(CPP_get_openmp_threads());
    return rcpp_result_gen;
END_RCPP
}
// CPP_set_openmp_threads
void CPP_set_openmp_threads(int n);
RcppExport SEXP _FractalTools_CPP_set_openmp_threads(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    CPP_set_openmp_threads(n);
    return R_NilValue;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_FractalTools_BigArmaEuc", (DL_FUNC) &_FractalTools_BigArmaEuc, 2},
    {"_FractalTools_CPP_get_openmp_threads", (DL_FUNC) &_FractalTools_CPP_get_openmp_threads, 0},
    {"_FractalTools_CPP_set_openmp_threads", (DL_FUNC) &_FractalTools_CPP_set_openmp_threads, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_FractalTools(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
