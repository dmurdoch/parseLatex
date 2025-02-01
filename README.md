
<!-- README.md is generated from README.Rmd. Please edit that file -->

# parseLatex

<!-- badges: start -->

[![R-CMD-check](https://github.com/dmurdoch/parseLatex/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dmurdoch/parseLatex/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/parseLatex)](https://CRAN.R-project.org/package=parseLatex)
<!-- badges: end -->

The goal of `parseLatex` is to provide a parser for a subset of LaTeX
syntax that is more complete than what is handled by the
`tools::parseLatex()` parser.

Perhaps some day it will handle all LaTeX inputs, but that’s not likely.
For now, I’m aiming to handle anything that `knitr::kable()` and the
`kableExtra` functions will produce, plus related code.

A website describing the current state is here:
<https://dmurdoch.github.io/parseLatex/> .

## Installation

You can install the development version of parseLatex from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("dmurdoch/parseLatex")
```

## Example

This is a basic example.

``` r
library(parseLatex)
library(kableExtra)
latex <- kbl(mtcars[1:2, 1:2], format = "latex")
cat(latex)
#> 
#> \begin{tabular}[t]{l|r|r}
#> \hline
#>   & mpg & cyl\\
#> \hline
#> Mazda RX4 & 21 & 6\\
#> \hline
#> Mazda RX4 Wag & 21 & 6\\
#> \hline
#> \end{tabular}
parsed <- parseLatex(latex)
# This is a blank followed by a table; drop the blank
table <- parsed[[find_env(parsed, "tabular")]]
# Get the alignment options from the content
brace_options(get_contents(table))
#> {l|r|r}
tableCell(table, 2,2) # The title counts!
#>  21
tableCell(table, 2,2) <- "Changed!"
table
#> ENVIRONMENT: \begin{tabular}[t]{l|r|r}
#> \hline
#>   & mpg & cyl\\
#> \hline
#> Mazda RX4 & Changed! & 6\\
#> \hline
#> Mazda RX4 Wag & 21 & 6\\
#> \hline
#> \end{tabular}
```

## To-do list

- Possibly add class for table objects, and implement indexing and
  subsetting on that class.
- See what `kableExtra` does with tables, and try to make that easier.
- Do something with math environments?
