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
#' - Optional support for capturing `...`.
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
#' @param column_transform `[logical(1)]`
#'
#'   Should a single `i` argument represent column subsetting? For example,
#'   this standardizes `x[i]` to `x[,j]`. This is useful when creating data
#'   frame `[` methods, but array subsetting likely won't use this feature.
#'
#' @param env `[environment]`
#'
#'   Generally, the calling environment of the `[` method. Used to detect
#'   the number of arguments supplied when the user invoked the `[` method.
#'
#' @param fn `[function]`
#'
#'   Generally, the function corresponding to the `[` method. Used to check
#'   that the `[` method is valid, and to count the number of arguments
#'   alongside `env`.
#'
#' @return
#' A named list of the following 4 elements containing information about the
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
#' - `transformed`: A logical of size 1 indicating whether the transformation
#'   of `x[i]` to `x[,j]` was performed or not.
#'
#' @export
#' @examples
#' # Generally you would replace `slicer()` with your `[.myclass` method,
#' # which you would then call as `x[i]`. This just collects the subscript info
#' # and returns a compact view of it. Normally you would use the processed
#' # results to slice `x`.
#'
#' slicer <- function(x, i, j, ..., drop = FALSE) {
#'   info <- collect_subscripts(i, j, ...)
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
#' slicer(x, , 1)
#'
#' # This would be equivalent to calling `x[]`
#' slicer(x, )
#'
#' # Extra `...` are standardized too. Again, implicit dimensions are returned
#' # as `NULL`.
#' slicer(x, 1, 2, 3)
#' slicer(x, 1, 2, , 3)
#'
#'
#' # You don't have to supply `...` if you don't need it. This
#' # won't be as useful because it isn't as complex to count here, but it
#' # will still standardize `x[i]`, which is nice.
#' slicer_simple <- function(x, i, j) {
#'   collect_subscripts(i, j)
#' }
#'
#' slicer_simple(x, 1)
collect_subscripts <- function(i,
                               j,
                               ...,
                               column_transform = TRUE,
                               env = parent.frame(),
                               fn = sys.function(sys.parent())) {
  if (!is.environment(env)) {
    abort("`env` must be an environment.")
  }
  if (identical(env, globalenv())) {
    abort("`collect_subscripts()` can only be called from inside a function.")
  }
  if (!is.function(fn)) {
    abort("`fn` must be a function.")
  }
  if (!is_bool(column_transform)) {
    abort("`column_transform` must be a boolean value (TRUE / FALSE).")
  }

  fmls <- formals(fn)
  fml_names <- names(fmls)

  loc_subscripts <- match(c("i", "j", "..."), fml_names)
  loc_i <- loc_subscripts[[1]]
  loc_j <- loc_subscripts[[2]]
  loc_dots <- loc_subscripts[[3]]

  if (is.na(loc_i)) {
    abort("Caller function must have formal argument `i`.")
  }
  if (is.na(loc_j)) {
    abort("Caller function must have formal argument `j`.")
  }
  if (loc_j - loc_i != 1L) {
    abort("Caller function must place arguments `i` and `j` adjacent to each other, in that order.")
  }
  if (is.na(loc_dots)) {
    loc_subscripts <- loc_subscripts[1:2]
  }

  i_missing <- missing(i)
  j_missing <- missing(j)

  i <- subscript_standardize(i, i_missing)
  j <- subscript_standardize(j, j_missing)

  dots <- dots_standardize(environment())

  if (column_transform) {
    loc_extras <- index_invert(loc_subscripts)
    fml_extra_names <- fml_names[loc_extras]

    n_subscripts_used <- count_subscripts(fml_extra_names, env)
    transformed <- n_subscripts_used == 1L && !i_missing

    if (transformed) {
      j <- i
      i <- NULL
    }
  } else {
    transformed <- FALSE
  }

  list(
    i = i,
    j = j,
    dots = dots,
    transformed = transformed
  )
}

# ------------------------------------------------------------------------------

abort <- function(msg) {
  stop(msg, call. = FALSE)
}

# Must evaluate missingness of "extra" formals like `x` or `drop` in the
# environment in which they are first supplied. If any formals have a default,
# they are only considered missing in the environment in which they are first
# created. If you pass on a missing argument that has a default, it is no longer
# considered missing.
count_subscripts <- function(fml_extra_names, env) {
  expr <- quote(nargs())
  n_args_used <- eval_bare(expr, env)

  n_extras_used <- 0L

  # Count non-missing extras
  for (name in fml_extra_names) {
    expr <- bquote(missing(.(name)))
    missing <- eval_bare(expr, env)
    n_extras_used <- n_extras_used + as.integer(!missing)
  }

  n_subscripts_used <- n_args_used - n_extras_used

  n_subscripts_used
}

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

is_bool <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

index_invert <- function(x) {
  if (index_is_empty(x)) {
    TRUE
  } else {
    -x
  }
}

index_is_empty <- function(x) {
  !length(x) || all(x == 0L)
}
