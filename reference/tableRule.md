# Work with rules in tables

In LaTeX, "rules" are horizontal lines in a table. These functions let
rules be extracted or modified.

## Usage

``` r
find_rules(table)

rules(table, idx = find_rules(table))

find_rule(table, row)

rule(table, row)

rule(table, row, asis = FALSE, idx = find_rules(table)) <- value
```

## Arguments

- table:

  A tabular-like environment to work with.

- idx:

  A list of indices as produced by `find_rules()`.

- row:

  The rules will precede the contents of this row. The rule after the
  final row uses `row = tableNrow(table) + 1`.

- asis:

  Should a newline be added after the value? If `asis = TRUE`, it will
  not be.

- value:

  The content to be inserted into the cell. This can be a
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  object, or a character string that will be converted to one.

## Value

`find_rules()` returns a list of
[LaTeX2range](https://dmurdoch.github.io/parseLatex/reference/LaTeX2range.md)
objects giving the locations of the rules before each line. The last
item in the list gives the location of any rules after the last line.

`rules(table)` returns a list of the rules before each row. The last
entry will be the rule(s) following the last row.

`find_rule(table, row)` returns a
[LaTeX2range](https://dmurdoch.github.io/parseLatex/reference/LaTeX2range.md)
for the rule before `row`, not including the final whitespace.

`rule(table, row)` returns the rule(s) before `row`.

## See also

Use
[`index_to_path()`](https://dmurdoch.github.io/parseLatex/reference/path_to_index.md)
to convert to a path.

## Examples

``` r
latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
parsed <- parseLatex(latex)
table <- parsed[[find_tabular(parsed)]]
table <- prepare_table(table)
find_rules(table)
#> [[1]]
#> path=2,1 range=2:3
#> 
#> [[2]]
#> path=3,1 range=2:3
#> 
#> [[3]]
#> path=4,1 range=2:3
#> 
#> [[4]]
#> path=5,1 range=2:3
#> 

rules(table)
#> [[1]]
#> \hline
#> 
#> 
#> [[2]]
#> \hline
#> 
#> 
#> [[3]]
#> \hline
#> 
#> 
#> [[4]]
#> \hline
#> 
#> 

find_rule(table, 1)
#> path=2,1 range=2:2

rule(table, 1)
#> \hline

rule(table, 2) <- "\\midrule"
table
#> ENVIRONMENT: \begin{tabular}[t]{l|r|r}
#> \hline
#>   & mpg & cyl\\
#> \midrule
#> 
#> Mazda RX4 & 21 & 6\\
#> \hline
#> Mazda RX4 Wag & 21 & 6\\
#> \hline
#> \end{tabular}
```
