# Convert between paths and indices

Convert between paths and indices

## Usage

``` r
path_to_index(path, items)

index_to_path(index, items)

paths_to_ranges(path1, path2, items)

get_ranges(items, ranges)
```

## Arguments

- path:

  A vector of integers, assumed to be a path through "ITEMLIST" entries
  in a
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  or
  [LaTeX2item](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  object.

- items:

  The referenced object.

- index:

  A scalar integer which would be the index to an item if `items` was
  flattened.

- path1, path2:

  Paths into the same destination list.

- ranges:

  A list of
  [LaTeX2range](https://dmurdoch.github.io/parseLatex/reference/LaTeX2range.md)
  objects, such as that produced by `paths_to_ranges()`.

## Value

`path_to_index` returns a scalar value corresponding the the index if
`items` was flattened.

`index_to_path` returns a vector of integers which would index the
specified item.

`paths_to_range` returns a list of
[LaTeX2range](https://dmurdoch.github.io/parseLatex/reference/LaTeX2range.md)
objects covering all entries extending from `path1` to `path2`.

`get_ranges()` extracts the specified ranges, concatenates them, and
returns them as a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
object.

## See also

[`flatten_itemlists()`](https://dmurdoch.github.io/parseLatex/reference/itemlist.md)

## Examples

``` r
latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
parsed <- parseLatex(latex)
tablepath <- path_to(parsed, is_env, envtypes = "tabular")
table <- prepare_table(parsed[[tablepath]])
path_to_index(c(4,1,1), table)
#> [1] 34
index_to_path(3, table)
#> [1] 1 3

ranges <- paths_to_ranges(index_to_path(3, table),
                          c(4,1,1), table)
lapply(ranges, get_range, items = table)
#> [[1]]
#> ]{l|r|r}
#> 
#> [[2]]
#> 
#> \hline
#>   & mpg & cyl\\
#> 
#> [[3]]
#> 
#> \hline
#> Mazda RX4 & 21 & 6\\
#> 
#> [[4]]
#> 
#> 
#> 

get_ranges(table, ranges)
#> ]{l|r|r}
#> \hline
#>   & mpg & cyl\\
#> \hline
#> Mazda RX4 & 21 & 6\\
#> 
```
