slicer <- function(x, i, j, ..., drop = FALSE) {
  collect_subscripts(i, j, ..., drop = drop)
}

slicer_no_transform <- function(x, i, j, ..., drop = FALSE) {
  collect_subscripts(i, j, ..., drop = drop, column_transform = FALSE)
}

slicer_no_drop <- function(x, i, j, ...) {
  collect_subscripts(i, j, ...)
}

slicer_no_dots_no_drop <- function(x, i, j) {
  collect_subscripts(i, j)
}
