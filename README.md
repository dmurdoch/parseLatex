
<!-- README.md is generated from README.Rmd. Please edit that file -->

# parseLatex

<!-- badges: start -->
<!-- badges: end -->

The goal of `parseLatex` is to provide a parser for a subset of LaTeX
syntax that is more capable than the `tools::parseLatex()` parser.

## Installation

You can install the development version of parseLatex from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("dmurdoch/parseLatex")
```

## Example

This is a basic example :

``` r
library(parseLatex)
library(kableExtra)
parseLatex(kbl(mtcars[1:2, 1:2]))
#> NULL
```
