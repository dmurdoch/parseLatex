# Lists of items

Lists of items

## Usage

``` r
new_itemlist(...)

flatten_itemlists(items, recursive = FALSE)

placeholder()

show_itemlists(items, indent = 0, verbose = FALSE)
```

## Arguments

- ...:

  Items to be passed to
  [`latex2()`](https://dmurdoch.github.io/parseLatex/reference/as_LaTeX2.md).

- items:

  A list of
  [LaTeX2item](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  objects.

- recursive:

  Whether to proceed recursively.

- indent:

  How much to indent the display?

- verbose:

  Whether to show tags of non-itemlists and details of each itemlist.

## Value

`new_itemlist()` returns an `ITEMLIST` item containing the items.

`flatten_itemlists()` returns `items` with `ITEMLIST` items expanded. If
`items` itself was an `ITEMLIST`, it is returned as a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
object; otherwise its type will be unchanged. The result will never
include any `ITEMLIST` or `PLACEHOLDER` items at the top level, and if
`recursive` is `TRUE`, not at any level.

`placeholder()` returns a `LaTeX2item` object with tag `PLACEHOLDER`.
These will never print, and are used as spacers within an `ITEMLIST`.

`show_itemlists()` is a debugging function called for the side effect of
displaying the itemlist structure of an object.

## Details

An `ITEMLIST` is a list of items. Deparsing it just concatenates the
parts. This is intended to be used when parsing tables, for example,
where it makes sense to break up the table into individual rows. See
[prepare_table](https://dmurdoch.github.io/parseLatex/reference/prepare_table.md)
for more details.

## Examples

``` r
new_itemlist(parseLatex("abc def"), label = "items")
#> ITEMLIST: abc defitems
```
