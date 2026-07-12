# Ranges within LaTeX2 lists.

Ranges within LaTeX2 lists.

## Usage

``` r
LaTeX2range(path, range)

# S3 method for class 'LaTeX2range'
print(x, source = NULL, ...)
```

## Arguments

- path:

  An integer vector to use as a path.

- range:

  A range of values within the path.

- x:

  Object to print.

- source:

  Optional parsed list from which to extract the range.

- ...:

  Ignored.

## Value

`LaTeX2range()` returns a constructed `LaTeX2range` object.

## Details

LaTeX2range objects are lists with `path` and `range` entries. `path` is
a recursive index into a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
list, and `range` is a range of entries in the result.

If `path` is `NULL`, the object refers to the entire source object. If
`range` is `NULL`, it refers to the whole
[LaTeX2item](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
given by the `path`.
