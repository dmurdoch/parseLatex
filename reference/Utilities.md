# Miscellaneous utilities

Miscellaneous utilities

## Usage

``` r
drop_items(items, which)

select_items(items, which)

drop_whitespace(items)

trim_whitespace(items)

include_whitespace(items, which)

split_chars(item, split = "")

new_block(...)

new_env(name, ...)
```

## Arguments

- items:

  A
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  object or list of items, or a
  [LaTeX2item](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  which is a list.

- which:

  A
  [LaTeX2range](https://dmurdoch.github.io/parseLatex/reference/LaTeX2range.md)
  object describing which items to operate on, or a vector of indices
  into `items`.

- item:

  A non-list
  [LaTeX2item](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md).

- split:

  Where to split the characters.

- ...:

  Items to be passed to
  [`latex2()`](https://dmurdoch.github.io/parseLatex/reference/as_LaTeX2.md).

- name:

  The desired environment name.

## Value

`drop_items()` returns the list of items with specific items removed.

`select_items()` returns the list of subsetted items.

`drop_whitespace()` returns the items with whitespace (blanks, tabs,
newlines) removed.

`trim_whitespace()` returns the items with leading and trailing
whitespace (blanks, tabs, newlines) removed.

`include_whitespace()` returns `which` with following whitespace
(blanks, tabs, newlines) included.

`split_chars()` returns a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
list containing the result of calling
[strsplit](https://rdrr.io/r/base/strsplit.html) on the text of the
`item`.

`new_block()` returns a `BLOCK` item containing the items.

`new_env()` returns an environment item containing the other items.

## Note

`drop_whitespace()` will drop the whitespace that separates text items,
so deparsing will merge them into a single item.

## See also

`drop_whitespace()` does not act recursively; use
[reduce_whitespace](https://dmurdoch.github.io/parseLatex/reference/reduce_whitespace.md)
for that.

## Examples

``` r
parsed <- parseLatex("Hello")
unclass(parsed)
#> [[1]]
#> TEXT: Hello
#> 
unclass(split_chars(parsed[[1]]))
#> [[1]]
#> TEXT: H
#> 
#> [[2]]
#> TEXT: e
#> 
#> [[3]]
#> TEXT: l
#> 
#> [[4]]
#> TEXT: l
#> 
#> [[5]]
#> TEXT: o
#> 
new_block(parseLatex("abc"))
#> BLOCK: {abc}
new_env("itemize", parseLatex("\\item An item"))
#> ENVIRONMENT: \begin{itemize}\item An item\end{itemize}
```
