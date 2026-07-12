# Find path to a particular kind of item

Find path to a particular kind of item

## Usage

``` r
path_to(items, test, ..., all = FALSE)

get_item(items, path = index_to_path(index, items), index)

get_items(items, paths = lapply(indices, index_to_path, items), indices)

set_item(items, path, value)

insert_values(items, path, values, after = FALSE)

get_container(items, path)

get_which(path)
```

## Arguments

- items:

  A list of latex items.

- test:

  Which test function to use.

- ...:

  Additional parameters to pass to `is_fn`.

- all:

  Return all paths, or just the first?

- path:

  Integer vector of subitems

- index:

  Index into the flattened version of `items`.

- paths:

  List of paths

- indices:

  Vector of indices into the flattened version of `items`.

- value:

  A
  [LaTeX2item](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  to set as a value.

- values:

  A
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  list or a
  [LaTeX2item](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md).

- after:

  If `TRUE`, insert the values after `path`.

## Value

`path_to()` returns the recursive path to the first example matching the
`is_fn` conditions, or a list of paths to all matching items.

`get_item()` returns the item at the given path. If `index` is
specified, `get_item()` will return that item in the flattened version
of `items`.

`get_items()` returns the items at the given paths as a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
object. If `index` is specified, `get_items()` will return those items
in the flattened version of `items`.

`set_item()` replaces the item at the given path, and returns the
modified version of `items`.

`insert_values()` inserts the `values` before the item mentioned in
`path` (or after if requested), and returns the modified version of
`items`.

`get_container()` returns the item containing the given path

`get_which()` returns the index of the item within its container.

## Details

`path_to()` does a recursive search in the order items appear in the
deparse.

## Examples

``` r
latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex", caption = "Sample table")
parsed <- parseLatex(latex)
parsed
#> \begin{table}
#> 
#> \caption{Sample table}
#> \centering
#> \begin{tabular}[t]{l|r|r}
#> \hline
#>   & mpg & cyl\\
#> \hline
#> Mazda RX4 & 21 & 6\\
#> \hline
#> Mazda RX4 Wag & 21 & 6\\
#> \hline
#> \end{tabular}
#> \end{table}
path <- path_to(parsed, test = is_env,
                        envtypes = "tabular")
get_item(parsed, path)
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
