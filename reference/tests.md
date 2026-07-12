# Test objects

Test objects

## Usage

``` r
is_env(item, envtypes = NULL)

is_macro(item, macros = NULL)

is_block(item)

is_bracket(item, bracket)

is_whitespace(item)

is_text(item)

is_error(item)

is_itemlist(item)

is_placeholder(item)

is_char(item, char)

is_catcode(item, code)
```

## Arguments

- item:

  An object of class
  [LaTeX2item](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  to test.

- envtypes:

  Types of Latex environment to check for, e.g. `"table"`.

- macros:

  Which macros to match, e.g. `"\\\\caption"`.

- bracket:

  Which bracket are we looking for?

- char:

  A character to match

- code:

  A catcode to match

## Value

`is_env()` returns a boolean if the item matches.

`is_macro()` returns a boolean indicating the match.

`is_block()` returns a boolean indicating whether the `item` is a block
wrapped in curly braces.

`is_bracket()` returns a boolean indicating that the `item` is a bracket
of the specified type.

`is_whitespace()` returns a boolean indicating if the `item` is a space,
tab or newline.

`is_text()` returns a boolean indicating if the `item` is text.

`is_error()` returns a boolean indicating if the `item` is an error.

`is_itemlist()` returns a boolean indicating if the `item` is an
[ITEMLIST](https://dmurdoch.github.io/parseLatex/reference/itemlist.md)
item.

`is_placeholder()` returns a boolean indicating if the `item` is a
[PLACEHOLDER](https://dmurdoch.github.io/parseLatex/reference/itemlist.md)
item.

`is_char()` returns a boolean indicating if the `item` is a `SPECIAL`
matching `char`.

`is_catcode()` returns a boolean indicating if the `item` is a `SPECIAL`
with the given catcode.

## Examples

``` r
is_bracket(parseLatex("[]")[[1]], "[")
#> [1] TRUE
```
