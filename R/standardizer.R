#' Standardize slicing indices
#'
#' @description
#' `standardizer()` standardizes slicing indices to make it easier to develop
#' `[` methods. It takes care of turning missing indices into explicit
#' `NULL`s, tells you how many indexing arguments were provided by the user,
#' and standardizes `x[i]` into `x[,j]`.
#'
#' @details
#'
#'
#' @param i `[vector]`
#'
#'   A vector of indices
#'
standardizer <- function(i, j, ..., drop = NULL, env = parent.frame()) {
  if (n_dots(...) != 0L) {
    stop("`...` must be empty.", call. = FALSE)
  }
  if (identical(env, globalenv())) {
    stop("`standardizer()` must be called from within a function.", call. = FALSE)
  }

  # Ignore `x` when counting the number of args
  n_x <- 1L
  n_args <- eval_bare(quote(nargs()), env)
  n_args <- n_args - n_x

  i_missing <- eval_bare(quote(missing(i)), env)
  j_missing <- eval_bare(quote(missing(j)), env)

  # Allows `drop` to be optional.
  # Assumes that if the user's signature has `drop`, then it is given
  # a default like `drop = FALSE`. A `drop` with no default that isn't
  # provided will look like it is missing here.
  if (missing(drop)) {
    drop_missing <- TRUE
  } else {
    drop_missing <- eval_bare(quote(missing(drop)), env)
  }

  i <- arg_standardize(i, i_missing)
  j <- arg_standardize(j, j_missing)
  dots <- dots_standardize(env)

  # Ignore `drop` when determining how to standardize `x[i]`
  n_drop <- !drop_missing
  n_indexers <- n_args - n_drop

  if (n_indexers == 1L && !i_missing) {
    j <- i
    i <- NULL
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

arg_standardize <- function(x, missing) {
  if (missing) {
    NULL
  } else if (is.null(x)) {
    integer()
  } else {
    x
  }
}

dots_standardize <- function(env) {
  .Call(export_dots_standardize, env)
}
