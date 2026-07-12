# Work with table cells

These functions work with the content of cells in tabular-like
environments. Cells are numbered with the first row (typically column
titles) being row 1. Rules (i.e. horizontal lines) are not considered
part of a cell.

## Usage

``` r
find_tableCell(table, row, col)

tableCell(table, row, col)

tableCell(table, row, col, asis = FALSE) <- value
```

## Arguments

- table:

  A tabular-like environment to work with.

- row, col:

  row and column in the table.

- asis:

  Should blanks be added around the value?

- value:

  The content to be inserted into the cell. This can be a
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  object, or a character string that will be converted to one.

## Value

`find_tableCell()` returns a
[LaTeX2range](https://dmurdoch.github.io/parseLatex/reference/LaTeX2range.md)
object giving the location of the requested cell.

`tableCell()` returns a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
object containing all of the table content in the cell (but not the &).

## Details

`find_tableCell()` returns `NULL` if the requested cell is missing
because an earlier cell covered multiple columns. It signals an error if
a request is made beyond the bounds of the table.

Unless `asis = TRUE`, `tableContent(table) <- value` will add blanks at
the start end end if not present, to make the result more readable.

If `col` is higher than the current table width, the assignment will
fail with an error. If only `row` is too high, blank lines will be added
and it should succeed.

## Examples

``` r
latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
parsed <- parseLatex(latex)
table <- prepare_table(parsed[[find_tabular(parsed)]], do_cells = TRUE)
find_tableCell(table, 1, 2)
#> path=2,3 range=all

tableCell(table, 1, 2)
#> mpg

tableCell(table, 5, 2) <- " d "
table
#> ENVIRONMENT: \begin{tabular}[t]{l|r|r}
#> \hline
#>   & mpg & cyl\\
#> \hline
#> Mazda RX4 & 21 & 6\\
#> \hline
#> Mazda RX4 Wag & 21 & 6\\
#>  &  & \\
#>  & d & \\
#> \hline
#> \end{tabular}
```
