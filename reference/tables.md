# Functions related to parsing LaTeX tables

Functions related to parsing LaTeX tables

## Usage

``` r
is_tabular(item)

find_tabular(items, start = 1)
```

## Arguments

- item:

  An item from a
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  list object.

- items:

  A
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  list object.

- start:

  Where to start looking.

## Value

`is_tabular()` returns boolean indicating if this is a tabular-like
environment.

`find_tabular()` returns the index of the first tabular-like
environment, or `NA` if none is found.

## Examples

``` r
latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
parsed <- parseLatex(latex)
is_tabular(parsed[[2]])
#> [1] TRUE


find_tabular(parsed)
#> [1] 2
table <- parsed[[find_tabular(parsed)]]
table
#> ENVIRONMENT: \begin{tabular}[t]{l|r|r}
#> \hline
#>   & mpg & cyl\\
#> \hline
#> Mazda RX4 & 21 & 6\\
#> \hline
#> Mazda RX4 Wag & 21 & 6\\
#> \hline
#> \end{tabular}
```
