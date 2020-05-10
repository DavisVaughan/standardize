# ------------------------------------------------------------------------------
# `x[]`

test_that("can do a no argument subset", {
  x <- slicer()

  expect_identical(x$i, NULL)
  expect_identical(x$j, NULL)
})

test_that("extra args don't get in the way", {
  x <- slicer_extras("a", drop = TRUE)

  expect_identical(x$i, NULL)
  expect_identical(x$j, NULL)
})

# ------------------------------------------------------------------------------
# `x[,]`

test_that("can have missing `i` and `j`", {
  x <- slicer(,)

  expect_identical(x$i, NULL)
  expect_identical(x$j, NULL)
})

# ------------------------------------------------------------------------------
# `x[i]`

test_that("`i` only slicing is transformed to `j`", {
  x <- slicer(1)

  expect_identical(x$i, NULL)
  expect_identical(x$j, 1)
})

test_that("`i` only transformation can be turned off", {
  x <- slicer_no_transform(1)

  expect_identical(x$i, 1)
  expect_identical(x$j, NULL)
})

test_that("extra args don't interfere with column transform", {
  x <- slicer_extras("a", 1, drop = TRUE)

  expect_identical(x$j, 1)
})

# ------------------------------------------------------------------------------
# `x[i,]`

test_that("`i,` is interpreted as row subsetting", {
  x <- slicer(1,)

  expect_identical(x$i, 1)
  expect_identical(x$j, NULL)
})

test_that("extra args don't interfere", {
  x <- slicer_extras("a", 1, , drop = TRUE)

  expect_identical(x$i, 1)
  expect_identical(x$j, NULL)
})

# ------------------------------------------------------------------------------
# `x[,j]`

test_that("`,j` is column subsetting", {
  x <- slicer(, 1)

  expect_identical(x$i, NULL)
  expect_identical(x$j, 1)
})

test_that("can select `j` by name", {
  x <- slicer(j = 1)

  expect_identical(x$i, NULL)
  expect_identical(x$j, 1)
})

# ------------------------------------------------------------------------------
# `x[i, j]`

test_that("`i, j` works", {
  x <- slicer(1, 2)

  expect_identical(x$i, 1)
  expect_identical(x$j, 2)
})

test_that("naming the arguments reverses them", {
  x <- slicer(j = 1, i = 2)

  expect_identical(x$i, 2)
  expect_identical(x$j, 1)
})

# ------------------------------------------------------------------------------
# `x[,, k]`

test_that("can subset into 3rd dimension", {
  x <- slicer(,,)

  expect_identical(x$i, NULL)
  expect_identical(x$j, NULL)
  expect_identical(x$dots, list(NULL))
})

test_that("can subset into 3rd dimension", {
  x <- slicer(,,1)

  expect_identical(x$i, NULL)
  expect_identical(x$j, NULL)
  expect_identical(x$dots, list(1))
})

# ------------------------------------------------------------------------------
# `x[,,,k]`

test_that("implicit dots are transformed into `NULL`", {
  x <- slicer(,,,1)
  y <- slicer(,,1,)
  z <- slicer(,,,,)

  expect_identical(x$dots, list(NULL, 1))
  expect_identical(y$dots, list(1, NULL))
  expect_identical(z$dots, list(NULL, NULL, NULL))
})

# ------------------------------------------------------------------------------
# `NULL` subscripts

test_that("`NULL` i and j are transformed to `integer()`", {
  x <- slicer(NULL,)
  y <- slicer(,NULL)

  expect_identical(x$i, integer())
  expect_identical(y$j, integer())
})

test_that("`NULL` dots are transformed to `integer()`", {
  x <- slicer(,,NULL)

  expect_identical(x$dots, list(integer()))
})

# ------------------------------------------------------------------------------
# no dots

test_that("dots are optional", {
  x <- slicer_no_dots()

  expect_identical(x$i, NULL)
  expect_identical(x$j, NULL)
  expect_identical(x$dots, list())

  x <- slicer_no_dots(1)

  expect_identical(x$i, NULL)
  expect_identical(x$j, 1)
  expect_identical(x$dots, list())

  x <- slicer_no_dots(1,)

  expect_identical(x$i, 1)
  expect_identical(x$j, NULL)
  expect_identical(x$dots, list())

  x <- slicer_no_dots(,1)

  expect_identical(x$i, NULL)
  expect_identical(x$j, 1)
  expect_identical(x$dots, list())
})

# ------------------------------------------------------------------------------
# Misc

test_that("can handle any number of extra args in the caller", {
  slicer <- function(x, i, j, ..., foo = 1, bar, drop = FALSE) {
    collect_subscripts(i, j, ...)
  }

  x <- slicer("a", 1)

  expect_identical(x$i, NULL)
  expect_identical(x$j, 1)
  expect_identical(x$dots, list())

  x <- slicer("a", 1,)

  expect_identical(x$i, 1)
  expect_identical(x$j, NULL)
  expect_identical(x$dots, list())
})

test_that("can be doubly wrapped and manually passed env and fn", {
  wrapper <- function(i, j, ..., env = parent.frame(), fn = sys.function(sys.parent())) {
    collect_subscripts(i, j, ..., env = env, fn = fn)
  }

  slicer <- function(x, i, j, ..., drop = FALSE) {
    wrapper(i, j, ...)
  }

  x <- slicer("a", 1)

  expect_identical(x$i, NULL)
  expect_identical(x$j, 1)
  expect_identical(x$dots, list())

  x <- slicer("a", 1,)

  expect_identical(x$i, 1)
  expect_identical(x$j, NULL)
  expect_identical(x$dots, list())
})
