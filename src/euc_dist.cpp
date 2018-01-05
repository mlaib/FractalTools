#define ARMA_NO_DEBUG

#include <Rcpp.h>
#include <armadillo>
// [[Rcpp::depends(RcppArmadillo, BH, bigmemory)]]

using namespace Rcpp;
using namespace arma;

#include <bigmemory/BigMatrix.h>
#include <omp.h>
extern int openmp_threads;



// C++11 plugin
// [[Rcpp::plugins(cpp11)]]


NumericVector sortIt(NumericVector v){
  int len = v.size();
  std::sort(&v[0], &v[len], std::greater<int>());
  return v;
}

template <typename T>
void BigArmaEuclidean(const Mat<T>& inBigMat, Mat<T> outBigMat) {
  
  int W = inBigMat.n_rows;
#pragma omp parallel for                                                     \
  if (openmp_threads > 1 &&  W > 1000)                                       \
    num_threads(openmp_threads)                                              \
    //shared(outBigMat)
    for(int i = 0; i < W; i++){
      for(int j = i; j < W; j++){
        if (i != j){
          outBigMat(i,i)=1000;
          outBigMat(j,i) = outBigMat(i,j) = sqrt(sum(pow((inBigMat.row(i) - inBigMat.row(j)),2)));
        }
      }
    }
}


// [[Rcpp::export]]
void BigArmaEuc(SEXP pInBigMat, SEXP pOutBigMat) {

  XPtr<BigMatrix> xpMat(pInBigMat);
  XPtr<BigMatrix> xpOutMat(pOutBigMat);
  
  
  //int type = xpMat->matrix_type();
  BigArmaEuclidean(
    arma::Mat<double>((double *)xpMat->matrix(), xpMat->nrow(), xpMat->ncol(), false),
    arma::Mat<double>((double *)xpOutMat->matrix(), xpOutMat->nrow(), xpOutMat->ncol(), false)
  );
  return;
}




// If _OPENMP is included:

#ifdef _OPENMP
#include <omp.h>
#endif

int openmp_threads = 1;

// [[Rcpp::export]]
DataFrame CPP_get_openmp_threads() {
  int num_threads = openmp_threads;
#ifdef _OPENMP
  int max_threads = omp_get_max_threads();
#else
  int max_threads = 0;
#endif
  DataFrame res =
    DataFrame::create(_["available"] = max_threads > 0,
                      _["max"] = max_threads,
                      _["threads"] = num_threads);
  res.attr("row.names") = "OpenMP";
  return res;
}

// [[Rcpp::export]]
void CPP_set_openmp_threads(int n) {
  if (n < 1) stop("internal error -- number of threads must be >= 1");
#ifdef _OPENMP
  int max_threads = omp_get_max_threads();
  if (n > max_threads) n = max_threads;
  openmp_threads = n;
#else
  if (n > 1) Rf_warning("OpenMP support not available");
  openmp_threads = 1;
#endif
}