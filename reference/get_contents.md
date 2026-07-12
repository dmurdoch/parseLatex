# Convenience functions to get or set contents of item

Convenience functions to get or set contents of item

## Usage

``` r
get_contents(item)

set_contents(item, value)
```

## Arguments

- item:

  An item from a Latex list (or a
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  list with one item).

- value:

  An object that can be coerced to be a
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  object.

## Value

`get_contents` returns the contents of the item as a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
list.

`set_contents` returns the original `item` with the contents replaced by
`value`.

## Examples

``` r
get_contents(parseLatex("{abc}"))
#> abc

set_contents(parseLatex("{abc}"), "def")
#> BLOCK: {def}
```
