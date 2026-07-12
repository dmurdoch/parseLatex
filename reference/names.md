# Utility functions finding names and types of objects

Utility functions finding names and types of objects

## Usage

``` r
latexTag(item)

catcode(item)

envName(item)

envName(item) <- value

macroName(item)
```

## Arguments

- item:

  A
  [LaTeX2item](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  which is an environment

- value:

  A character string to set as the name

## Value

`latexTag()` returns the
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
tag for the item or `NULL`.

`catcode()` returns the TeX catcode for the item, or `NULL`.

`envName()` returns the Latex environment name for an item, or `NULL`.

`macroName()` returns the Latex macro, or `NULL`.
