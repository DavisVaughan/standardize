#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP export_eval_bare(SEXP, SEXP);
extern SEXP export_dots_standardize(SEXP dots);

static const R_CallMethodDef CallEntries[] = {
  {"export_eval_bare",        (DL_FUNC) &export_eval_bare, 2},
  {"export_dots_standardize", (DL_FUNC) &export_dots_standardize, 1},
  {NULL, NULL, 0}
};

void R_init_standardize(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
