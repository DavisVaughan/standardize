# ------------------------------------------------------------------------------
# `x[]`

test_that("can do a no argument subset", {
  x <- slicer()

  expect_identical(x$i, NULL)
  expect_identical(x$j, NULL)
  expect_identical(x$transformed, FALSE)
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
  expect_identical(x$transformed, FALSE)
})

# ------------------------------------------------------------------------------
# `x[i]`

test_that("`i` only slicing is transformed to `j`", {
  x <- slicer(1)

  expect_identical(x$i, NULL)
  expect_identical(x$j, 1)
  expect_identical(x$transformed, TRUE)
})

test_that("`i` only transformation can be turned off", {
  x <- slicer_no_transform(1)

  expect_identical(x$i, 1)
  expect_identical(x$j, NULL)
  expect_identical(x$transformed, FALSE)
})

test_that("extra args don't interfere with column transform", {
  x <- slicer_extras("a", 1, drop = TRUE)

  expect_identical(x$j, 1)
  expect_identical(x$transformed, TRUE)
})

# ------------------------------------------------------------------------------
# `x[i,]`

test_that("`i,` is interpreted as row subsetting", {
  x <- slicer(1,)

  expect_identical(x$i, 1)
  expect_identical(x$j, NULL)
  expect_identical(x$transformed, FALSE)
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
  expect_identical(x$transformed, FALSE)
})

test_that("can select `j` by name", {
  x <- slicer(j = 1)

  expect_identical(x$i, NULL)
  expect_identical(x$j, 1)
  expect_identical(x$transformed, FALSE)
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
# named dots

test_that("names are kept on dots", {
  x <- slicer(1, 2, a = 3, 4, b = 5)
  expect_named(x$dots, c("a", "", "b"))
})

test_that("names are backfilled with empty strings", {
  x <- slicer(1, 2, 3, a = 4)
  expect_named(x$dots, c("", "a"))
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

# ------------------------------------------------------------------------------
# Errors

test_that("`env` must be an environment", {
  expect_error(collect_subscripts(env = "x"), "must be an environment")
})

test_that("Can't call `collect_subscripts()` at the top level", {
  # Hack to get it to evaluate at the top level since `expect_error()` enquos
  expect_error(
    eval(quote(collect_subscripts()), globalenv()),
    "only be called from inside a function"
  )
})

test_that("`fn` must be a function", {
  slicer <- function(i, j, ...) {
    collect_subscripts(i, j, ..., fn = "x")
  }

  expect_error(slicer(1), "must be a function")
})

test_that("`column_transform` must be a boolean", {
  slicer <- function(i, j, ...) {
    collect_subscripts(i, j, ..., column_transform = "x")
  }

  expect_error(slicer(1), "must be a boolean value")
})

test_that("`i` must be in the signature", {
  verify_errors({
    slicer <- function(j, ...) {
      collect_subscripts(j = j, ...)
    }

    expect_error(slicer(1))
  })
})

test_that("`j` must be in the signature", {
  verify_errors({
    slicer <- function(i, ...) {
      collect_subscripts(i, ...)
    }

    expect_error(slicer(1))
  })
})

test_that("i and j must be adjacent in the signature", {
  # Note: This check keeps `x[i,]` from counting incorrectly.
  # It assumes `j` is next.
  verify_errors({
    slicer <- function(i, k, j, ...) {
      collect_subscripts(i, j, ...)
    }

    expect_error(slicer(1))
    expect_error(slicer(1,))
  })
})


test_that("collect_subscripts() has informative errors", {
  verify_output(test_path("errors", "test-collect-subscripts.txt"), {
    "# `i` must be in the signature"
    slicer <- function(j, ...) {
      collect_subscripts(j = j, ...)
    }

    slicer(1)

    "# `j` must be in the signature"
    slicer <- function(i, ...) {
      collect_subscripts(i, ...)
    }

    slicer(1)

    "# i and j must be adjacent in the signature"
    slicer <- function(i, k, j, ...) {
      collect_subscripts(i, j, ...)
    }

    slicer(1)
    slicer(1,)
  })
})

