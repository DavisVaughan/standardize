
<!-- README.md is generated from README.Rmd. Please edit that file -->

# standardize

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/DavisVaughan/standardize/branch/master/graph/badge.svg)](https://codecov.io/gh/DavisVaughan/standardize?branch=master)
[![R build
status](https://github.com/DavisVaughan/standardize/workflows/R-CMD-check/badge.svg)](https://github.com/DavisVaughan/standardize/actions)
<!-- badges: end -->

The goal of standardize is to ease the creation of data frame and array
`[` methods by providing a way to standardize `i`, `j`, and `...` and
provide information about the user’s `[` call.

## Installation

You can install the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("DavisVaughan/standardize")
```

## Example

``` r
library(standardize)
```

When you create a `[` method for data frames, it generally has a
signature like this:

``` r
`[.foo_df` <- function(x, i, j, drop = FALSE) {
  
}
```

This seems straightforward, but get’s a little confusing when you need
to tell if `i`, `j`, and `drop` have been supplied by the user, or are
missing. For example, if `i` is supplied but `j` is missing, like in
`x[i]`, then you probably want to perform some kind of column subsetting
operation, rather than row subsetting. But if `x[i,]` is supplied, this
is actually a row subset, and `j` is considered to be present but empty.

The goal of standardize is to help with this by standardizing these
arguments in such a way that it makes figuring out what to do with them
trivial. To do this, add `collect_subscripts()` as the first line of
your `[` method and pass it all the subscript related information.

``` r
`[.foo_df` <- function(x, i, j, drop = FALSE) {
  info <- collect_subscripts(i, j)
  str(info)
}
```

`collect_subscripts()` will do all of the counting for you, and will
interpret calls like `x[i]` as `x[,j]` so that you as the developer just
have to focus on the values of `i` and `j`. You’ll interpret `NULL`
values to mean that your user didn’t supply that subscript. For example:

``` r
df <- data.frame(x = 1:5)
class(df) <- c("foo_df", class(df))

# Column subsetting, standardized to `i = NULL, j = 1`
df[1]
#> List of 3
#>  $ i   : NULL
#>  $ j   : num 1
#>  $ dots: list()
#> NULL

# Row subsetting with implicit `j = NULL`
df[1,]
#> List of 3
#>  $ i   : num 1
#>  $ j   : NULL
#>  $ dots: list()
#> NULL

# Still considered column subsetting
df[1, drop = FALSE]
#> List of 3
#>  $ i   : NULL
#>  $ j   : num 1
#>  $ dots: list()
#> NULL
```

It also helps with more complex array subsetting. In this case, you also
need to count higher dimensional subsetting in the `...`. Again,
implicit dimensions should be returned as `NULL` to imply that they were
counted but are missing. With array subsetting, you probably don’t want
`x[i]` to be interpreted as `x[,j]`, so you can turn that off with
`column_transform = FALSE`.

``` r
`[.foo_array` <- function(x, i, j, ..., drop = FALSE) {
  info <- collect_subscripts(i, j, ..., column_transform = FALSE)
  str(info)
}
```

``` r
x <- array(1:5)
class(x) <- c("foo_array", class(x))

# Not interpreted as `x[,j]`
x[1]
#> List of 3
#>  $ i   : num 1
#>  $ j   : NULL
#>  $ dots: list()
#> NULL

# Compare the following results, see the implicit `NULL` in the 3rd dimension?
x[1, 2]
#> List of 3
#>  $ i   : num 1
#>  $ j   : num 2
#>  $ dots: list()
#> NULL
x[1, 2,]
#> List of 3
#>  $ i   : num 1
#>  $ j   : num 2
#>  $ dots:List of 1
#>   ..$ : NULL
#> NULL

# Things can get a little crazy in high dimensional space, but this should
# be fairly interpretable.
x[1, 2, , 3, , 5, drop = TRUE]
#> List of 3
#>  $ i   : num 1
#>  $ j   : num 2
#>  $ dots:List of 4
#>   ..$ : NULL
#>   ..$ : num 3
#>   ..$ : NULL
#>   ..$ : num 5
#> NULL
```
