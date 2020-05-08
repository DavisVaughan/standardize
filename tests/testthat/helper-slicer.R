slicer <- function(x, i, j, ..., drop = FALSE) {
  collect_subscripts()
}

slicer_no_transform <- function(x, i, j, ..., drop = FALSE) {
  collect_subscripts(column_transform = FALSE)
}

slicer_no_drop <- function(x, i, j, ...) {
  collect_subscripts()
}

slicer_no_dots_no_drop <- function(x, i, j) {
  collect_subscripts()
}
