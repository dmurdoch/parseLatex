# Find or modify macro or environment options

Many Latex environments and macros take optional parameters wrapped in
square brackets. `find_bracket_options` finds those, assuming they come
immediately after the macro.

Some Latex environments and macros take optional parameters wrapped in
curly brackets (braces). `find_brace_options` finds those if they
immediately follow the environment or macro (and possibly some bracketed
options).

## Usage

``` r
find_bracket_options(items, which = 1L, start = 1L)

bracket_options(items, which = 1L, start = 1L)

bracket_options(items, which = 1, start = 1, asis = FALSE) <- value

find_brace_options(items, which = 1L, start = 1L, path = FALSE)

brace_options(items, which = 1, start = 1)

brace_options(items, which = 1, start = 1, asis = FALSE) <- value
```

## Arguments

- items:

  A list of latex items.

- which:

  Which options do you want? Some macros support more than one set.

- start:

  Start looking at `items[[start]]`. `start` may be a path.

- asis:

  Should newlines be added around the value?

- value:

  The content to be inserted into the cell. This can be a
  [LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  object, or a character string that will be converted to one.

- path:

  If `TRUE`, return a path rather than an index, as with
  [`find_general()`](https://dmurdoch.github.io/parseLatex/reference/finders.md),

## Value

`find_bracket_options` returns a
[LaTeX2range](https://dmurdoch.github.io/parseLatex/reference/LaTeX2range.md)
object pointing to the options within `items` (including the brackets).

`bracket_options` returns a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
object containing the specified options.

`find_brace_options` returns the index or path to the block containing
the options.

`brace_options` returns a
[LaTeX2](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
object containing the specified options.

## Examples

``` r
parsed <- parseLatex("\\section[a]{b}")
macro <- find_macro(parsed, "\\section")
bracket_options(parsed, start = macro + 1)
#> \section[a]{b}

bracket_options(parsed, start = macro + 1) <- "Short Title"
parsed
#> \section[Short Title][a]{b}

brace_options(parsed, start = macro + 1)
#> {b}

brace_options(parsed, start = macro + 1) <- "Long Title"
parsed
#> \section[Short Title][a]{Long Title}
```
