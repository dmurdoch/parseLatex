# Functions to work with rows in tables

Functions to work with rows in tables

## Usage

``` r
find_tableRow(table, row, withExtras = FALSE, withData = TRUE)

tableRow(table, row, withExtras = FALSE, withData = TRUE)

tableRow(table, row, asis = FALSE, withExtras = FALSE, withData = TRUE) <- value
```

## Arguments

- table:

  A tabular-like environment to work with.

- row:

  row in the table (1 is top row), including rows of labels.

- withExtras:

  If `TRUE`, include the extras before the line of data, such as
  `\hline`, etc.

- withData:

  If `TRUE`, include the data.

- asis:

  Should a linebreak and newline be added after the value?

- value:

  The content to be inserted into the cell. This can be a
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  object, or a character string that will be converted to one.

## Value

`find_tableRow()` returns a
[LaTeX2range](https://dmurdoch.github.io/parseLatex/reference/LaTeX2range.md)
of the entries corresponding to the content of row i of the table.

`tableRow()` returns a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
object containing all of the table content in the row.

## Details

Unless `asis = TRUE`, `tableContent(table) <- value` will add "\\ and a
newline at the end if not present.

If the `row` value is higher than the number of rows in the table, blank
rows will be added to fill the space between.

If `withExtras = TRUE` and you want the result to start on a new line,
you need to add the newline explicitly in `value` when using the
assignment function.

## Examples

``` r
latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
parsed <- parseLatex(latex)
table <- parsed[[find_tabular(parsed)]]
find_tableRow(table, 1)
#> path= range=8:16

tableRow(table, 1)
#>   & mpg & cyl\\
tableRow(table, 1, withExtras = TRUE)
#> 
#> \hline
#>   & mpg & cyl\\

tableRow(table, 5) <- "a & b & c"
table
#> ENVIRONMENT: \begin{tabular}[t]{l|r|r}
#> \hline
#>   & mpg & cyl\\
#> \hline
#> Mazda RX4 & 21 & 6\\
#> \hline
#> Mazda RX4 Wag & 21 & 6\\
#>  &  & \\
#> a & b & c\\
#> \hline
#> \end{tabular}
```
