
<!-- README.md is generated from README.Rmd. Please edit that file -->

# parseLatex

<!-- badges: start -->
<!-- badges: end -->

The goal of `parseLatex` is to provide a parser for a subset of LaTeX
syntax that is more complete than the `tools::parseLatex()` parser.

Perhaps some day it will handle all LaTeX inputs, but that’s not likely.
For now, I’m aiming to handle anything that `knitr::kable()` and the
`kableExtra` functions will produce.

At the moment, it doesn’t even do as much as `tools::parseLatex()`.

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
parsed
#> NULL
getParseData(parsed)
#> NULL
```
