# Functions relating to the data content of a table

Functions relating to the data content of a table

## Usage

``` r
find_tableContent(table)

tableContent(table)

tableContent(table, asis = FALSE) <- value
```

## Arguments

- table:

  A tabular-like environment to work with. It must not be one for which
  [`prepare_table()`](https://dmurdoch.github.io/parseLatex/reference/prepare_table.md)
  has been called.

- asis:

  Should newlines be added around the value?

- value:

  The content to be inserted into the cell. This can be a
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  object, or a character string that will be converted to one.

## Value

`find_tableContent()` returns the indices of the entries corresponding
to content of the table.

`tableContent()` returns a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
object containing all of the table content after the options.

## Details

Unless `asis = TRUE`, `tableContent(table) <- value` will add newlines
at the start end end if not present, to make the result more readable.

## Examples

``` r
latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
parsed <- parseLatex(latex)
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
tableContent(table)
#> 
#> \hline
#>   & mpg & cyl\\
#> \hline
#> Mazda RX4 & 21 & 6\\
#> \hline
#> Mazda RX4 Wag & 21 & 6\\
#> \hline
#> 

tableContent(table) <- "Mazda RX4 & 21 & 6\\\\"
table
#> ENVIRONMENT: \begin{tabular}[t]{l|r|r}
#> Mazda RX4 & 21 & 6\\
#> \end{tabular}
tableContent(table, asis = TRUE) <- "Mazda RX4 & 21 & 6\\\\"
table
#> ENVIRONMENT: \begin{tabular}[t]{l|r|r}Mazda RX4 & 21 & 6\\\end{tabular}
```
