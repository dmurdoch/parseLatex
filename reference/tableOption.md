# Functions related to table options.

Functions related to table options.

## Usage

``` r
find_posOption(table)

posOption(table)

posOption(table, asis = FALSE) <- value

find_widthOption(table)

widthOption(table)

widthOption(table, asis = FALSE) <- value

find_columnOptions(table)

columnOptions(table)

columnOption(table, column)

columnOptions(table, asis = FALSE) <- value

columnOption(table, column) <- value
```

## Arguments

- table:

  A known tabular-like environment object, or the contents of one.

- asis:

  Whether to make small modifications in replacement functions.

- value:

  A character string or
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  object.

- column:

  For which column?

## Value

`find_posOption()` returns the indices of the entries corresponding to
the "pos" option, including the brackets, within the table.

`posOption()` returns a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
object containing the "pos" option.

`find_widthOption()` returns the index of the block corresponding to the
"width" option, if there is one. Only some tabular-like environments
have these.

`widthOption()` returns a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
object containing the "width" option, if the table has one.

`find_columnOptions()` returns a
[LaTeX2range](https://dmurdoch.github.io/parseLatex/reference/LaTeX2range.md)
object for the column options of the table.

`columnOptions()` returns a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
object containing the "column" options.

`columnOption()` returns a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
object containing the requested column option. A `"|"` divider will not
be included.

## Details

Unless `asis == TRUE`, the value for `value` in
`posOption(table) <- value` can be specified with or without the
enclosing brackets.

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
find_posOption(table)
#> path= range=1:3

posOption(table)
#> [t]

posOption(table) <- "h"
posOption(table)
#> [h]
find_widthOption(table)
#> NULL

widthOption(table)
#> NULL

find_columnOptions(table)
#> path= range=4:4
columnOptions(table)
#> {l|r|r}

columnOption(table, 3)
#> r
columnOptions(table) <- "lrr"
table
#> ENVIRONMENT: \begin{tabular}[h]{lrr}
#> \hline
#>   & mpg & cyl\\
#> \hline
#> Mazda RX4 & 21 & 6\\
#> \hline
#> Mazda RX4 Wag & 21 & 6\\
#> \hline
#> \end{tabular}
columnOption(table, 3) <- "p{1cm}"
columnOptions(table)
#> {lrp{1cm}}
```
