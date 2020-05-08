#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdbool.h>


static R_len_t dots_size(SEXP dots);
static SEXP dots_standardize(SEXP dots, SEXP env, R_len_t size);

// Standardize dots by replacing missing dots with `NULL`,
// but otherwise evaluate the dots and return the results
// as a list (possibly named).
SEXP export_dots_standardize(SEXP env) {
  SEXP dots = Rf_findVar(R_DotsSymbol, env);
  R_len_t size = dots_size(dots);
  SEXP out = dots_standardize(dots, env, size);
  return out;
}

// -----------------------------------------------------------------------------

static R_len_t dots_size(SEXP dots) {
  if (dots == R_UnboundValue) {
    // No dots at all in the environment
    return 0;
  } else if (dots == R_MissingArg) {
    // Dots are present, but none were supplied
    return 0;
  } else {
    return Rf_length(dots);
  }
}

// -----------------------------------------------------------------------------

static inline SEXP dot_eval(SEXP dot, SEXP env);

static void dots_standardize_named(SEXP out,
                                   SEXP node,
                                   SEXP env,
                                   R_len_t size,
                                   R_len_t i);

static SEXP dots_standardize(SEXP dots, SEXP env, R_len_t size) {
  SEXP out = PROTECT(Rf_allocVector(VECSXP, size));
  SEXP node = dots;

  for (R_len_t i = 0; i < size; ++i, node = CDR(node)) {
    if (TAG(node) != R_NilValue) {
      dots_standardize_named(out, node, env, size, i);
      break;
    }

    SEXP dot = CAR(node);
    dot = dot_eval(dot, env);

    SET_VECTOR_ELT(out, i, dot);
  }

  UNPROTECT(1);
  return out;
}

static void dots_standardize_named(SEXP out,
                                   SEXP node,
                                   SEXP env,
                                   R_len_t size,
                                   R_len_t i) {
  SEXP names = PROTECT(Rf_allocVector(STRSXP, size));
  SEXP* p_names = STRING_PTR(names);

  // Backfill with `""`
  for (R_len_t j = 0; j < i; ++j) {
    p_names[j] = R_BlankString;
  }

  // Continue dot-eval loop where we left off
  for (; i < size; ++i, node = CDR(node)) {
    SEXP tag = TAG(node);

    if (tag == R_NilValue) {
      p_names[i] = R_BlankString;
    } else {
      p_names[i] = PRINTNAME(tag);
    }

    SEXP dot = CAR(node);
    dot = dot_eval(dot, env);

    SET_VECTOR_ELT(out, i, dot);
  }

  Rf_setAttrib(out, R_NamesSymbol, names);

  UNPROTECT(1);
}

// Missing dot positions are replaced with `NULL`,
// otherwise the promise is evaluated.
static inline SEXP dot_eval(SEXP dot, SEXP env) {
  if (dot == R_MissingArg) {
    return R_NilValue;
  } else {
    return Rf_eval(dot, env);
  }
}
