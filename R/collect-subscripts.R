#' Standardize subscripts
#'
#' @description
#' `collect_subscripts()` standardizes subscripts to make it easier to
#' develop `[` methods, especially for data frame and array subsetting.
#' It helps with a number of things, including:
#'
#' - Counting the number of subscript arguments supplied.
#'
#' - Transforming implicit missing subscripts into explicit `NULL` values.
#'
#' - Optional support for capturing `...` and `drop`.
#'
#' - Optional support for standardizing the data frame column subsetting
#'   behavior of `x[i]` into `x[,j]`.
#'
#' `collect_subscripts()` can __only__ be called from inside of a function.
#' Calling `collect_subscripts()` in the console directly will result in
#' an error.
#'
#' The expected way to use `collect_subscripts()` is as the first line of
#' a `[` method. You'll collect the processed subscripts, and then use
#' them to slice the input accordingly.
#'
#' @details
#' When counting arguments, `collect_subscripts()` assumes that 1 argument
#' is reserved for `x` and will subtract it from the total number of arguments.
#' If you are implementing a `[` method, this is automatically enforced by the
#' method signature. If you are calling `collect_subscripts()` from somewhere
#' else, keep this in mind when inspecting the results.
#'
#' If `NULL` is provided as a subscript, it is standardized to `integer()` to
#' prevent confusion with `NULL` values in the resulting info list. A `NULL`
#' subscript should have the same meaning as `integer()`.
#'
#' @param i,j `[vector]`
#'
#'   Required vectors of subscripts.
#'
#' @param ... `[dots]`
#'
#'   Optional additional subscripts for subsetting into higher dimensions. Names
#'   are preserved. Implicit missing subscripts are transformed into explicit
#'   `NULL` values.
#'
#'   If no `...` are provided, the return value will contain an empty list
#'   in the `$dots` slot.
#'
#' @param drop `[NULL / logical(1)]`
#'
#'   Optional argument to capture `drop`. If your `[` method signature
#'   doesn't include a `drop` argument, don't supply this argument. If
#'   it does, it is required that `drop` has a default value in your method. It
#'   is recommended to default this to `drop = FALSE` in your method.
#'
#' @param column_transform `[logical(1)]`
#'
#'   Should a single `i` argument represent column subsetting? For example,
#'   this standardizes `x[i]` to `x[,j]`. This is useful when creating data
#'   frame `[` methods, but array subsetting likely won't use this feature.
#'
#' @return
#' A named list of the following 7 elements containing information about the
#' standardized subscripts.
#'
#' - `i`: The standardized `i` subscript. If `NULL`, `i` is considered to be
#'   missing.
#'
#' - `j`: The standardized `j` subscript. If `NULL`, `j` is considered to be
#'   missing.
#'
#' - `dots`: A list of the standardized `...` subscripts. If no `...` are
#'   supplied, an empty list is returned. If `...` are supplied and any
#'   subscripts are missing, `NULL` values are returned in those locations.
#'   For example, `x[i, j, , k]` would return a list of `list(NULL, k)`.
#'
#' - `drop`: The standardized `drop` argument. If `drop` is not provided,
#'   this is `NULL`.
#'
#' - `missing`: A named list of 3 elements, `i`, `j`, and `drop`, each of which
#'   are logical vectors of size 1 stating whether or not the argument was
#'   supplied by the user. Note that if `x[i]` is standardized to `x[,j]`,
#'   then `i` won't be missing even though its value is `NULL`. Additionally,
#'   `j` will be missing even though it will have a value.
#'
#' - `n_args`: The total number of subscript-related arguments supplied. This
#'   includes all arguments in the `[` call _except_ for the object being
#'   subset, `x`. Missing arguments are counted here. For example, `x[1,]`
#'   has a `n_args` value of `2` because `i` is supplied and `j` is supplied
#'   but missing. On the other hand, `x[i]` has a `n_args` value of `1`.
#'
#' - `n_subscripts`: The total number of subscript arguments supplied. This
#'   counts `i`, `j`, and `...` arguments. It is only different drop `n_args`
#'   if `drop` is directly supplied by the end user.
#'
#' @export
#' @examples
#' # Generally you would replace `slicer()` with your `[.myclass` method,
#' # which you would then call as `x[i]`. This just collects the subscript info
#' # and returns a compact view of it. Normally you would use the processed
#' # results to slice `x`.
#'
#' slicer <- function(x, i, j, ..., drop = FALSE) {
#'   info <- collect_subscripts(i, j, ..., drop = drop)
#'   str(info)
#' }
#'
#' x <- "a"
#'
#' # Standardize an easy case
#' slicer(x, 1:5, 2)
#'
#' # By default, supplying just `i` standardizes `x[i]` to `x[,j]`
#' slicer(x, 1)
#'
#' # With just `j`, the implicit missing `i` value is returned as a `NULL`.
#' # Note that while `i` and `j` are the same for this and `slicer(x, 1)`,
#' # the `n_*` counters are different.
#' slicer(x, , 1)
#'
#' # This would be equivalent to calling `x[]`
#' slicer(x, )
#'
#' # Extra `...` are standardized too. Again, implicit dimensions are returned
#' # as `NULL`.
#' slicer(x, 1, 2, 3)
#'
#' slicer(x, 1, 2, , 3)
#'
#'
#' # You don't have to supply `...` and `drop` if you don't need it. This
#' # won't be as useful because it isn't as complex to count here, but it
#' # will still standardize `x[i]`, which is nice.
#' slicer_simple <- function(x, i, j) {
#'   collect_subscripts(i, j)
#' }
#'
#' # Note that `drop` is returned as `NULL` here, since it isn't in your
#' # function signature.
#' slicer_simple(x, 1)
collect_subscripts <- function(i, j, ..., drop = NULL, column_transform = TRUE) {
  env <- parent.frame()
  fmls <- formals(sys.function(sys.parent()))

  if (identical(env, globalenv())) {
    stop("`collect_subscripts()` must be called from within a function.", call. = FALSE)
  }
  if (!has_name(fmls, "i")) {
    stop("Caller function must have formal argument `i`.", call. = FALSE)
  }
  if (!has_name(fmls, "j")) {
    stop("Caller function must have formal argument `j`.", call. = FALSE)
  }

  # Ignore `x` when counting the number of args
  n_x <- 1L
  n_args <- eval_bare(quote(nargs()), env)
  n_args <- n_args - n_x

  i_missing <- missing(i)
  j_missing <- missing(j)

  i <- subscript_standardize(i, i_missing)
  j <- subscript_standardize(j, j_missing)

  # Evaluate the dots supplied in this function, not in the parent.frame().
  # We could look them up in the parent frame, but this forces the caller to
  # pass the `...` through, which I think is good practice.
  dots <- dots_standardize(environment())

  drop_info <- drop_standardize(drop, env, fmls)
  drop <- drop_info$drop
  drop_missing <- drop_info$drop_missing

  # Ignore `drop` when computing the number of subscripts supplied
  n_subscripts <- n_args - !drop_missing

  if (column_transform) {
    if (n_subscripts == 1L && !i_missing) {
      j <- i
      i <- NULL
    }
  }

  missing <- list(
    i = i_missing,
    j = j_missing,
    drop = drop_missing
  )

  list(
    i = i,
    j = j,
    dots = dots,
    drop = drop,
    missing = missing,
    n_args = n_args,
    n_subscripts = n_subscripts
  )
}

# ------------------------------------------------------------------------------

subscript_standardize <- function(x, missing) {
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

drop_standardize <- function(drop, env, fmls) {
  out <- list(drop = NULL, drop_missing = TRUE)

  # If `drop` isn't in the signature, don't try and find it
  if (!has_name(fmls, "drop")) {
    return(out)
  }

  # If `drop` is in the signature, it must have a default value, otherwise
  # forcing it here will throw an error.
  if (is_missing(fmls$drop)) {
    stop("A `drop` argument must be supplied with a default value.")
  }

  # Figure out if `drop` was user-supplied in the actual subscript call.
  # We can't check `missing(drop)` directly. Since `drop` has a default
  # value, it is only ever maybe missing in the function environment
  # where it is supplied. Otherwise it is always non-missing
  out$drop_missing <- eval_bare(quote(missing(drop)), env)

  out$drop <- force(drop)

  out
}

has_name <- function(x, name) {
  name %in% names(x)
}

is_missing <- function(x) {
  identical(x, missing_arg())
}

missing_arg <- function() {
  alist(missing = )$missing
}
