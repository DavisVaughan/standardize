# ------------------------------------------------------------------------------
# `x[]`

test_that("can do a no argument subset", {
  x <- slicer("a")

  expect_identical(x$i, NULL)
  expect_identical(x$j, NULL)
})

test_that("`drop` doesn't get in the way", {
  x <- slicer("a", drop = TRUE)

  expect_identical(x$i, NULL)
  expect_identical(x$j, NULL)
  expect_identical(x$drop, TRUE)
  expect_identical(x$n_args, 1L)
  expect_identical(x$n_indexers, 0L)
})

# ------------------------------------------------------------------------------
# `x[,]`

test_that("can have missing `i` and `j`", {
  x <- slicer("a", ,)

  expect_identical(x$i, NULL)
  expect_identical(x$j, NULL)
  expect_identical(x$n_args, 2L)
  expect_identical(x$n_indexers, 2L)
})

# ------------------------------------------------------------------------------
# `x[i]`

test_that("`i` only slicing is transformed to `j`", {
  x <- slicer("a", 1)

  expect_identical(x$i, NULL)
  expect_identical(x$j, 1)
  expect_identical(x$n_args, 1L)
  expect_identical(x$n_indexers, 1L)
})

test_that("`i` only transformation can be turned off", {
  x <- slicer_no_transform("a", 1)

  expect_identical(x$i, 1)
  expect_identical(x$j, NULL)
  expect_identical(x$n_args, 1L)
  expect_identical(x$n_indexers, 1L)
})

test_that("`drop` doesn't interfere with column transform", {
  x <- slicer("a", 1, drop = TRUE)

  expect_identical(x$j, 1)
  expect_identical(x$drop, TRUE)
  expect_identical(x$n_args, 2L)
  expect_identical(x$n_indexers, 1L)
})

# ------------------------------------------------------------------------------
# `x[i,]`

test_that("`i,` is interpreted as row subsetting", {
  x <- slicer("a", 1,)

  expect_identical(x$i, 1)
  expect_identical(x$j, NULL)
  expect_identical(x$n_args, 2L)
  expect_identical(x$n_indexers, 2L)
})

test_that("drop doesn't interfere", {
  x <- slicer("a", 1, , drop = TRUE)

  expect_identical(x$i, 1)
  expect_identical(x$j, NULL)
  expect_identical(x$drop, TRUE)
  expect_identical(x$n_args, 3L)
  expect_identical(x$n_indexers, 2L)
})

# ------------------------------------------------------------------------------
# `x[,j]`

test_that("`,j` is column subsetting", {
  x <- slicer("a", , 1)

  expect_identical(x$i, NULL)
  expect_identical(x$j, 1)
  expect_identical(x$n_args, 2L)
  expect_identical(x$n_indexers, 2L)
})

# ------------------------------------------------------------------------------
# `x[i, j]`

test_that("`i, j` works", {
  x <- slicer("a", 1, 2)

  expect_identical(x$i, 1)
  expect_identical(x$j, 2)
  expect_identical(x$n_args, 2L)
  expect_identical(x$n_indexers, 2L)
})

test_that("naming the arguments reverses them", {
  x <- slicer("a", j = 1, i = 2)

  expect_identical(x$i, 2)
  expect_identical(x$j, 1)
  expect_identical(x$n_args, 2L)
  expect_identical(x$n_indexers, 2L)
})



