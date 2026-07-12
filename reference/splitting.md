# Splitting lists of items

Splitting lists of items

## Usage

``` r
split_list(items, splits, include = FALSE)

split_latex(...)
```

## Arguments

- items:

  A
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  or similar list.

- splits:

  Which item numbers divide the parts?

- include:

  If `TRUE`, include the split item at the end of each part.

- ...:

  Arguments to pass to `split_list`.

## Value

`split_list()` returns a list of pieces separated at the splits.

`split_latex()` returns a list of pieces separated at the splits. Each
piece is marked as an `ITEMLIST` item, and the whole thing is also
marked that way.
