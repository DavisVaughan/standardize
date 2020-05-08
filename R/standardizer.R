standardizer <- function(i, ..., j = NULL, drop = NULL, env = parent.frame()) {
  if (n_dots(...) != 0L) {
    stop("`...` must be empty.")
  }

  # Ignore `x` when counting the number of args
  n_x <- 1L
  n_args <- eval_bare(quote(nargs()), env)
  n_args <- n_args - n_x

  i_missing <- eval_bare(quote(missing(i)), env)
  j_missing <- eval_bare(quote(missing(j)), env)
  drop_missing <- eval_bare(quote(missing(drop)), env)

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
