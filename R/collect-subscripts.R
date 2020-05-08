#' Standardize slicing indices
#'
#' @description
#' `collect_subscripts()` standardizes slicing indices to make it easier to
#' develop `[` methods. It takes care of turning missing indices into explicit
#' `NULL`s, tells you how many indexing arguments were provided by the user,
#' and optionally standardizes `x[i]` into `x[,j]`.
#'
#' @details
#'
#'
#' @param i `[vector]`
#'
#'   A vector of indices.
#'
collect_subscripts <- function(env = parent.frame(), column_transform = TRUE) {
  if (identical(env, globalenv())) {
    stop("`collect_subscripts()` must be called from within a function.", call. = FALSE)
  }

  # Ignore `x` when counting the number of args
  n_x <- 1L
  n_args <- eval_bare(quote(nargs()), env)
  n_args <- n_args - n_x

  i_missing <- eval_bare(quote(missing(i)), env)
  j_missing <- eval_bare(quote(missing(j)), env)

  i <- subscript_standardize("i", env, i_missing)
  j <- subscript_standardize("j", env, j_missing)
  dots <- dots_standardize(env)

  # Optionally find drop if it exists in the env.
  # Assumes that drop is provided with a default if it exists.
  # (This isn't perfect, because a local variable could be named `drop`,
  # but then the missing() check would error.)
  if ("drop" %in% names(env)) {
    drop_missing <- eval_bare(quote(missing(drop)), env)
    drop <- get("drop", envir = env)
  } else {
    drop_missing <- TRUE
    drop <- NULL
  }

  # Ignore `drop` when determining how to standardize `x[i]`
  n_drop <- !drop_missing
  n_indexers <- n_args - n_drop

  if (column_transform) {
    if (n_indexers == 1L && !i_missing) {
      j <- i
      i <- NULL
    }
  }

  list(
    i = i,
    j = j,
    dots = dots,
    drop = drop,
    n_args = n_args,
    n_indexers = n_indexers
  )
}

n_dots <- function(...) {
  nargs()
}

subscript_standardize <- function(arg, env, missing) {
  if (missing) {
    return(NULL)
  }

  x <- get(arg, envir = env, inherits = FALSE)

  if (is.null(x)) {
    integer()
  } else {
    x
  }
}

dots_standardize <- function(env) {
  .Call(export_dots_standardize, env)
}
