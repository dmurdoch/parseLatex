# Find a code sequence

Find a code sequence

## Usage

``` r
find_sequence(items, sequence, all = FALSE, ignore_whitespace = TRUE)

items_are_equal(items1, items2)
```

## Arguments

- items, sequence:

  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  objects or lists.

- all:

  Whether to return all matches, or just the first.

- ignore_whitespace:

  Whether to ignore whitespace in comparisons.

- items1, items2:

  Two
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  or
  [LaTeX2item](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  objects.

## Value

`find_sequence()` returns a
[LaTeX2range](https://dmurdoch.github.io/parseLatex/reference/LaTeX2range.md)
or list of them where `sequence` occurs within `items`.

`items_are_equal` returns a logical indicator of equality after removing
source references.

## Details

`find_sequence()` will only match sequences that are entirely contained
within a single list in `items`. Thus if
[`prepare_table()`](https://dmurdoch.github.io/parseLatex/reference/prepare_table.md)
is called on a table, then `find_sequence()` will find sequences in the
code before or after the rows, or entirely within a single cell, but not
crossing alignment markers (`"&"`).

## Examples

``` r
find_sequence(parseLatex("a & b & c"), "b & c")
#> path= range=5:9
```
