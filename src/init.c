#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP _FractalTools_BigArmaEuc(SEXP, SEXP);
extern SEXP _FractalTools_CPP_get_openmp_threads();
extern SEXP _FractalTools_CPP_set_openmp_threads(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_FractalTools_BigArmaEuc",             (DL_FUNC) &_FractalTools_BigArmaEuc,             2},
  {"_FractalTools_CPP_get_openmp_threads", (DL_FUNC) &_FractalTools_CPP_get_openmp_threads, 0},
  {"_FractalTools_CPP_set_openmp_threads", (DL_FUNC) &_FractalTools_CPP_set_openmp_threads, 1},
  {NULL, NULL, 0}
};

void R_init_FractalTools(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

