# Parse LaTeX code

The `parseLatex` function parses LaTeX source, producing a structured
object.

## Usage

``` r
parseLatex(
  text,
  verbose = FALSE,
  verbatim = c("verbatim", "verbatim*", "Sinput", "Soutput"),
  verb = "\\Sexpr",
  defcmd = c("\\newcommand", "\\renewcommand", "\\providecommand", "\\def",
    "\\let"),
  defenv = c("\\newenvironment", "\\renewenvironment"),
  catcodes = defaultCatcodes,
  recover = FALSE,
  showErrors = recover,
  ...
)
```

## Arguments

- text:

  A character vector containing LaTeX source code.

- verbose:

  If `TRUE`, print debug error messages.

- verbatim:

  A character vector containing the names of LaTeX environments holding
  verbatim text.

- verb:

  A character vector containing LaTeX macros that should be assumed to
  hold verbatim text.

- defcmd, defenv:

  Character vectors of macros that are assumed to define new macro
  commands or environments respectively. See the note below about some
  limitations.

- catcodes:

  A list or dataframe holding LaTeX "catcodes", such as
  [defaultCatcodes](https://dmurdoch.github.io/parseLatex/reference/defaultCatcodes.md).

- recover:

  If `TRUE`, attempt to recover from errors and continue parsing. See
  Details below.

- showErrors:

  If `TRUE`, show errors after parsing.

- ...:

  Additional parameters to pass to
  [showErrors](https://dmurdoch.github.io/parseLatex/reference/showErrors.md).

## Value

`parseLatex` returns parsed Latex in a list with class `"LaTeX2"`. Items
in the list have class `"LaTeX2item"`.

## Details

Some versions of LaTeX such as `pdflatex` only handle ASCII inputs,
while others such as `xelatex` allow Unicode input. `parseLatex` allows
Unicode input.

During processing of LaTeX input, an interpreter can change the handling
of characters as it goes, using the `\catcode` macro or others such as
`\makeatletter`. However, `parseLatex()` is purely a parser, not an
interpreter, so it can't do that, but the user can change handling for
the whole call using the `catcodes` argument.

`catcodes` should be a list or dataframe with at least two columns:

- `char` should be a column of single characters.

- `catcode` should be a column of integers in the range 0 to 15 giving
  the corresponding catcode.

During parsing, `parseLatex` will check these values first. If the input
character doesn't match anything, then it will be categorized:

- as a letter (catcode 11) using the ICU function
  `u_hasBinaryProperty(c, UCHAR_ALPHABETIC)` (or `iswalpha(c)` on
  Windows),

- as a control character (catcode 15) if its code point is less than 32,

- as "other" (catcode 12) otherwise.

When `recover = TRUE`, the parser will mark each error in the output,
and attempt to continue parsing. This may lead to a cascade of errors,
but will sometimes help in locating the first error. The section of text
related to the error will be marked as an item with tag `ERROR`.

## `defcmd` limitations

The LaTeX defining commands have fairly simple syntax, but `\def` and
`\let` from plain Tex have quite variable syntax and `parseLatex()` does
not attempt to handle it all. Stick with simple syntax like
`\def\bea{\begin{eqnarray}}` and it should work.

## See also

LaTeX2, LaTeX2item

## Examples

``` r
parsed <- parseLatex(r"(fran\c{c}ais)")
parsed
#> fran\c{c}ais
```
