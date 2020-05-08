#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

// rlang::eval_bare()
// Required so that `nargs()` evaluates in the correct environment.
// `base::eval()` is "stack inconsistent", see `?rlang::eval_bare`.
SEXP export_eval_bare(SEXP expr, SEXP env) {
  return Rf_eval(expr, env);
}
