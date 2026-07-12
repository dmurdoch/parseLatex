# Miscellaneous low-level finders

Miscellaneous low-level finders

## Usage

``` r
find_whitespace(items, ...)

find_env(items, envtypes = NULL, ...)

find_macro(items, macros = NULL, ...)

find_catcode(items, codes, ...)

find_tags(items, tags, ...)

find_char(items, char, ...)

find_block(items, ...)

find_general(items, test, ..., all = TRUE, path = FALSE)
```

## Arguments

- items:

  A list of latex items.

- ...:

  For `find_general`, additional arguments to pass to `test`. For the
  other `find_*` functions, additional arguments to pass to
  `find_general`.

- envtypes:

  Which types of environment to look for.

- macros:

  Which types of macros to look for.

- codes:

  Which codes to look for.

- tags:

  Which tags to look for.

- char:

  Which character to look for.

- test:

  Test function for target.

- all:

  If `FALSE`, return just the first match

- path:

  If `TRUE`, return a path rather than an index. See Details below.

## Value

`find_whitespace()` returns the indices of whitespace in `items`.

`find_env()` returns the indices within `items` of environments in
`envtypes`.

`find_macro()` returns the index within `items` of instances in
`macros`.

`find_catcode()` returns the index within `items`. of specials matching
`code`.

`find_tags()` returns the index within `items`. of items with tags
matching `tags`.

`find_char()` returns the index within `items` of characters matching
`char`. Only characters marked as SPECIAL by the parser will be found.

`find_block()` returns the index within `items` of blocks (i.e.
sequences in )

`find_general()` returns locations of objects matching the `test`.

## Details

These functions search through `items` for individual objects that match
a test. In general they do not operate recursively, with one exception.
If `items` contains `ITEMLIST` objects, the search will always recurse
into those.

By default the return value is an index or a vector of indices of the
matches. These are the indices as they would be if any `ITEMLIST`
objects had been flattened.

However, if `path = TRUE`, the path to the object will be returned. With
`all = FALSE`, this will be a numeric vector such that `items[[result]]`
is the matching item. With `all = TRUE` it will be a list of such
vectors.

## See also

[`index_to_path()`](https://dmurdoch.github.io/parseLatex/reference/path_to_index.md),
[`path_to_index()`](https://dmurdoch.github.io/parseLatex/reference/path_to_index.md)
