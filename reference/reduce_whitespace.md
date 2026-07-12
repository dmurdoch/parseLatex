# Remove excess whitespace recursively

Remove excess whitespace recursively

## Usage

``` r
reduce_whitespace(items, recursive = TRUE, all = FALSE)
```

## Arguments

- items:

  A
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  object.

- recursive:

  Apply to all lists within `items`.

- all:

  If `TRUE`, remove all white space, not just doubles.

## Value

`items` with double spaces or double newlines set to single, and
trailing spaces removed (or all whitespace removed, if `all` is `TRUE`).

## Examples

``` r
parsed <- parseLatex("a  {b\n\nc}")
parsed
#> a  {b
#> 
#> c}
reduce_whitespace(parsed)
#> a  {b
#> 
#> c}
```
