# Find a pattern in deparsed items

Searches a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
list for text using [`grepl()`](https://rdrr.io/r/base/grep.html) on
deparsed versions of parts of the code. It attempts to find the
narrowest match(es) that lie within a single container.

## Usage

``` r
find_pattern(items, pattern, ..., all = FALSE)
```

## Arguments

- items:

  A list of latex items.

- pattern:

  Pattern to use in [`grepl()`](https://rdrr.io/r/base/grep.html).

- ...:

  Additional parameters to pass to `grepl`.

- all:

  Find all matching, or the first?

## Value

`find_pattern()` returns a
[LaTeX2range](https://dmurdoch.github.io/parseLatex/reference/LaTeX2range.md)
object or (if `all` is `TRUE`) a list of them.

## Details

`find_pattern()` does a recursive search in the order items appear in
the deparse. If the pattern matches, it attempts to narrow the match by
recursing into containers and dropping earlier and later items. It
should always return syntactically correct LaTeX code in which the
pattern appears.

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
loc <- find_pattern(parsed, "RX4 Wag", fixed = TRUE)
loc
#> path=1,7 range=39:42
print(loc, source = parsed)
#> RX4 Wag
```
