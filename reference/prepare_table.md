# Split up a table by rows

Split up a table by rows

## Usage

``` r
prepare_table(table, do_cells = TRUE)

prepare_row(row)
```

## Arguments

- table:

  A tabular-like environment to work with.

- do_cells:

  Should the rows be prepared too?

- row:

  A list of items from a single row of a table.

## Value

A
[LaTeX2item](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
object which is the same table but with the contents divided into
[ITEMLIST](https://dmurdoch.github.io/parseLatex/reference/itemlist.md)s.
The first element is an
[ITEMLIST](https://dmurdoch.github.io/parseLatex/reference/itemlist.md)
holding everything before the first row, then each row is in its own
[ITEMLIST](https://dmurdoch.github.io/parseLatex/reference/itemlist.md),
and finally one more holding everything after the last row. The
attribute `has_itemlists` will be set to `TRUE`.

`prepare_row()` returns a
[LaTeX2item](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
object which is the same row with
[ITEMLIST](https://dmurdoch.github.io/parseLatex/reference/itemlist.md)s
holding the cells. The attribute `has_itemlist` will be set to `TRUE`.
The first list will be the "extras" at the start of the row; each cell
will be in the following
[ITEMLIST](https://dmurdoch.github.io/parseLatex/reference/itemlist.md)s.
The following cell delimiter will be included in the cell.

## Examples

``` r
latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
parsed <- parseLatex(latex)
table <- prepare_table(parsed[[find_tabular(parsed)]])
print(latex2(table), tags = TRUE)
#>  tag= ENVIRONMENT 
#> tabular:
#>     tag= ITEMLIST 
#>         tag= SPECIAL 
#> SPECIAL(OTHER): "["
#>         tag= TEXT 
#> TEXT:t
#>         tag= SPECIAL 
#> SPECIAL(OTHER): "]"
#>         tag= BLOCK 
#> {
#>             tag= TEXT 
#> TEXT:l
#>             tag= SPECIAL 
#> SPECIAL(OTHER): "|"
#>             tag= TEXT 
#> TEXT:r
#>             tag= SPECIAL 
#> SPECIAL(OTHER): "|"
#>             tag= TEXT 
#> TEXT:r
#>         }
#>     tag= ITEMLIST 
#>         tag= ITEMLIST 
#>             tag= SPECIAL 
#> SPECIAL(NEWLINE): "\n"
#>             tag= MACRO 
#> MACRO:\hline
#>             tag= SPECIAL 
#> SPECIAL(NEWLINE): "\n"
#>         tag= ITEMLIST 
#>             tag= SPECIAL 
#> SPECIAL(SPACE): "  "
#>             tag= SPECIAL 
#> SPECIAL(ALIGN): "&"
#>         tag= ITEMLIST 
#>             tag= SPECIAL 
#> SPECIAL(SPACE): " "
#>             tag= TEXT 
#> TEXT:mpg
#>             tag= SPECIAL 
#> SPECIAL(SPACE): " "
#>             tag= SPECIAL 
#> SPECIAL(ALIGN): "&"
#>         tag= ITEMLIST 
#>             tag= SPECIAL 
#> SPECIAL(SPACE): " "
#>             tag= TEXT 
#> TEXT:cyl
#>             tag= MACRO 
#> MACRO:\\
#>     tag= ITEMLIST 
#>         tag= ITEMLIST 
#>             tag= SPECIAL 
#> SPECIAL(NEWLINE): "\n"
#>             tag= MACRO 
#> MACRO:\hline
#>             tag= SPECIAL 
#> SPECIAL(NEWLINE): "\n"
#>         tag= ITEMLIST 
#>             tag= TEXT 
#> TEXT:Mazda
#>             tag= SPECIAL 
#> SPECIAL(SPACE): " "
#>             tag= TEXT 
#> TEXT:RX
#>             tag= SPECIAL 
#> SPECIAL(OTHER): "4"
#>             tag= SPECIAL 
#> SPECIAL(SPACE): " "
#>             tag= SPECIAL 
#> SPECIAL(ALIGN): "&"
#>         tag= ITEMLIST 
#>             tag= SPECIAL 
#> SPECIAL(SPACE): " "
#>             tag= SPECIAL 
#> SPECIAL(OTHER): "2"
#>             tag= SPECIAL 
#> SPECIAL(OTHER): "1"
#>             tag= SPECIAL 
#> SPECIAL(SPACE): " "
#>             tag= SPECIAL 
#> SPECIAL(ALIGN): "&"
#>         tag= ITEMLIST 
#>             tag= SPECIAL 
#> SPECIAL(SPACE): " "
#>             tag= SPECIAL 
#> SPECIAL(OTHER): "6"
#>             tag= MACRO 
#> MACRO:\\
#>     tag= ITEMLIST 
#>         tag= ITEMLIST 
#>             tag= SPECIAL 
#> SPECIAL(NEWLINE): "\n"
#>             tag= MACRO 
#> MACRO:\hline
#>             tag= SPECIAL 
#> SPECIAL(NEWLINE): "\n"
#>         tag= ITEMLIST 
#>             tag= TEXT 
#> TEXT:Mazda
#>             tag= SPECIAL 
#> SPECIAL(SPACE): " "
#>             tag= TEXT 
#> TEXT:RX
#>             tag= SPECIAL 
#> SPECIAL(OTHER): "4"
#>             tag= SPECIAL 
#> SPECIAL(SPACE): " "
#>             tag= TEXT 
#> TEXT:Wag
#>             tag= SPECIAL 
#> SPECIAL(SPACE): " "
#>             tag= SPECIAL 
#> SPECIAL(ALIGN): "&"
#>         tag= ITEMLIST 
#>             tag= SPECIAL 
#> SPECIAL(SPACE): " "
#>             tag= SPECIAL 
#> SPECIAL(OTHER): "2"
#>             tag= SPECIAL 
#> SPECIAL(OTHER): "1"
#>             tag= SPECIAL 
#> SPECIAL(SPACE): " "
#>             tag= SPECIAL 
#> SPECIAL(ALIGN): "&"
#>         tag= ITEMLIST 
#>             tag= SPECIAL 
#> SPECIAL(SPACE): " "
#>             tag= SPECIAL 
#> SPECIAL(OTHER): "6"
#>             tag= MACRO 
#> MACRO:\\
#>     tag= ITEMLIST 
#>         tag= ITEMLIST 
#>             tag= SPECIAL 
#> SPECIAL(NEWLINE): "\n"
#>             tag= MACRO 
#> MACRO:\hline
#>             tag= SPECIAL 
#> SPECIAL(NEWLINE): "\n"
#>         tag= TEXT 
#> TEXT:
row <- prepare_row(tableRow(table, 2))
print(latex2(row), tags = TRUE)
#>  tag= ITEMLIST 
#>     tag= TEXT 
#> TEXT:Mazda
#>     tag= SPECIAL 
#> SPECIAL(SPACE): " "
#>     tag= TEXT 
#> TEXT:RX
#>     tag= SPECIAL 
#> SPECIAL(OTHER): "4"
#>     tag= SPECIAL 
#> SPECIAL(SPACE): " "
#>     tag= SPECIAL 
#> SPECIAL(ALIGN): "&"
#>  tag= ITEMLIST 
#>     tag= SPECIAL 
#> SPECIAL(SPACE): " "
#>     tag= SPECIAL 
#> SPECIAL(OTHER): "2"
#>     tag= SPECIAL 
#> SPECIAL(OTHER): "1"
#>     tag= SPECIAL 
#> SPECIAL(SPACE): " "
#>     tag= SPECIAL 
#> SPECIAL(ALIGN): "&"
#>  tag= ITEMLIST 
#>     tag= SPECIAL 
#> SPECIAL(SPACE): " "
#>     tag= SPECIAL 
#> SPECIAL(OTHER): "6"
#>     tag= MACRO 
#> MACRO:\\
```
