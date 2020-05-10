slicer <- function(i, j, ...) {
  collect_subscripts(i, j, ...)
}

slicer_no_transform <- function(i, j, ...) {
  collect_subscripts(i, j, ..., column_transform = FALSE)
}

slicer_no_dots <- function(i, j) {
  collect_subscripts(i, j)
}

slicer_extras <- function(x, i, j, ..., drop = FALSE) {
  collect_subscripts(i, j, ...)
}
