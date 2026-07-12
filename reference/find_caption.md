# Find or drop captions

Find or drop captions

## Usage

``` r
find_caption(items)

drop_caption(items, idx = NULL)

path_to_caption(items)
```

## Arguments

- items:

  A
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  or other list of
  [LaTeX2item](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)s.

- idx:

  `NULL` or a vector of the same length as `items`

## Value

`find_caption()` returns a
[LaTeX2range](https://dmurdoch.github.io/parseLatex/reference/LaTeX2range.md)
object for any caption text, with an attribute `extra` holding the range
of associated macros and whitespace.

`drop_caption()` returns the `items` with captions dropped as a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
object. It has an attribute named `idx` that is the `idx` argument with
corresponding elements dropped.

`path_to_caption()` returns a path containing the location of the first
caption block within `items`. It has an attribute `extra` containing a
[LaTeX2range](https://dmurdoch.github.io/parseLatex/reference/LaTeX2range.md)
object for the associated macros and whitespace.

## Examples

``` r
parsed <- parseLatex("before \\caption{This is a caption} \\\\ after")
idx <- find_caption(parsed)
get_range(parsed, idx)
#> {This is a caption}
get_range(parsed, attr(idx, "extra"))
#> \caption{This is a caption} \\
drop_caption(parsed)
#> before  after
path_to_caption(parsed)
#> [1] 4
#> attr(,"extra")
#> path= range=3:6
```
