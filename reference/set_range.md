# Set items in a [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md) object

Set items in a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
object

## Usage

``` r
set_range(items, range, values)

get_range(items, range)
```

## Arguments

- items:

  A
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  object or other list of
  [LaTeX2item](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  objects.

- range:

  A
  [LaTeX2range](https://dmurdoch.github.io/parseLatex/reference/LaTeX2range.md)
  object.

- values:

  An object that can be coerced to a
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  object or (if `range$range` is `NULL`) a
  [LaTeX2item](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md).

## Value

`set_range()` replaces the item(s) at the given path, and returns the
modified version of `items`.

`get_range()` extracts the specified range and returns it as a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
object.

## Examples

``` r
latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex", caption = "Sample table")
parsed <- parseLatex(latex)
tablepath <- path_to(parsed, is_env, envtypes = "tabular")
range <- LaTeX2range(tablepath, 11)
parsed <- set_range(parsed, range, "The 11th item")
parsed
#> \begin{table}
#> 
#> \caption{Sample table}
#> \centering
#> \begin{tabular}[t]{l|r|r}
#> \hline
#>   & The 11th item & cyl\\
#> \hline
#> Mazda RX4 & 21 & 6\\
#> \hline
#> Mazda RX4 Wag & 21 & 6\\
#> \hline
#> \end{tabular}
#> \end{table}
get_range(parsed, range)
#> The
```
