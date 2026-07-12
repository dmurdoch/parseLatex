# Convert vector to table row and back

Convert vector to table row and back

## Usage

``` r
vector_to_row(cells, asis = FALSE, linebreak = TRUE)

row_to_vector(row, asis = FALSE, deparse = TRUE)
```

## Arguments

- cells:

  A list or vector of cell contents.

- asis:

  If `FALSE`, add or remove blanks around cell contents.

- linebreak:

  If `TRUE`, add a line break marker.

- row:

  A row from a table

- deparse:

  Should the result be deparsed?

## Value

`vector_to_row` returns a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
object which could be a row in a tabular object.

`row_to_vector` returns a character vector of the deparsed contents of
the row, or if `deparse` is `FALSE`, a list of the contents.

## Examples

``` r
vector_to_row(1:3)
#> 1 & 2 & 3\\
row_to_vector("1 & 2 & content \\\\")
#> [1] "1"       "2"       "content"
row_to_vector("1 & 2 & content \\\\", deparse = FALSE)
#> [[1]]
#> 1
#> 
#> [[2]]
#> 2
#> 
#> [[3]]
#> content
#> 
```
