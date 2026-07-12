# The parseLatex package

## Introduction

Parsing LaTeX is tricky, because LaTeX macros (in LaTeX packages, or in
user code) can change the parsing rules as they go.

`parseLatex` is not a LaTeX interpreter (at least mostly it isn’t, but
see the detailed comparison below), so it can’t do that: it uses the
same parsing rules for all code that it looks at. If you’re using a
LaTeX package that uses non-standard rules, you can use those, but they
have to apply to the whole section of code passed to
[`parseLatex()`](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md).

Subject to the limitation that the code only uses one set of rules,
`parseLatex` should be able to parse any LaTeX code. It extends the base
[`tools::parseLatex()`](https://rdrr.io/r/tools/parseLatex.html)
function in a few ways:

1.  It classifies every character in the source file according to TeX
    “catcodes”. The base function only handles some of them.
2.  The
    [`parseLatex::parseLatex()`](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
    function marks its output with class `"LaTeX2"` instead of
    `"LaTeX"`, and marks each item in the output with class
    `"LaTeX2item"`. This allows it to print things in a more readable
    way.
3.  The `parseLatex` package includes a large selection of functions for
    extracting and modifying parts of the parsed LaTeX.

More differences are listed below.

## Demo

A simple demonstration is in order.

First, we use `knitr` to create a LaTeX table.

``` r

library(knitr)
latex <- kable(mtcars[1:2, 1:2], format = "latex")
cat(latex)
#> 
#> \begin{tabular}{l|r|r}
#> \hline
#>   & mpg & cyl\\
#> \hline
#> Mazda RX4 & 21 & 6\\
#> \hline
#> Mazda RX4 Wag & 21 & 6\\
#> \hline
#> \end{tabular}
```

Next, we parse it in `parseLatex`.

``` r

library(parseLatex)
parsed <- parseLatex(latex)
```

Printing the result would appear to duplicate the input, but in fact it
is quite different. `parsed` is a list of class `"LaTeX2"`. Items in the
list are of class `"LaTeX2item"`. In this example, there are only two
items: the blank that `knitr` puts at the beginning of each table, and a
second entry which is the whole table environment:

``` r

parsed[[1]]
#> SPECIAL:
parsed[[2]]
#> ENVIRONMENT: \begin{tabular}{l|r|r}
#> \hline
#>   & mpg & cyl\\
#> \hline
#> Mazda RX4 & 21 & 6\\
#> \hline
#> Mazda RX4 Wag & 21 & 6\\
#> \hline
#> \end{tabular}
```

“`SPECIAL`” and “`ENVIRONMENT`” label the types of items. The table
environment contains the environment name, and a `"LaTeX2"` list
containing all the content.

If we hadn’t known where we put it, we could find the table location
using
[`find_env()`](https://dmurdoch.github.io/parseLatex/reference/finders.md):

``` r

find_env(parsed, "tabular")
#> [1] 2
```

We can extract the table, and use other functions to work with it:

``` r

table <- parsed[[find_env(parsed, "tabular")]]
# Get the alignment options from the content
columnOptions(table)
#> {l|r|r}
# To work with cells or rows in the table, we need
# to preprocess it:
table <- prepare_table(table, do_cells = TRUE)
tableCell(table, 2,2) # The title counts as a row
#> 21
tableCell(table, 1,1) <- "Model"
table
#> ENVIRONMENT: \begin{tabular}{l|r|r}
#> \hline
#> Model & mpg & cyl\\
#> \hline
#> Mazda RX4 & 21 & 6\\
#> \hline
#> Mazda RX4 Wag & 21 & 6\\
#> \hline
#> \end{tabular}
```

## Differences from `tools::parseLatex`

The parser in this package is based on the one used by the base R
[`tools::parseLatex`](https://rdrr.io/r/tools/parseLatex.html) function
(which I also wrote, based on other parsers in R). The output format is
similar, but not compatible. These are the main differences as of R
4.4.2. Several of these changes will be incorporated into R 4.5.0.

- In both this package and
  [`tools::parseLatex`](https://rdrr.io/r/tools/parseLatex.html), the
  result of calling the parser is a list of items.  
- The list has class `"LaTeX2"` in this package, and class `"LaTeX"` in
  [`tools::parseLatex`](https://rdrr.io/r/tools/parseLatex.html).
- All items have an attribute returned by the
  [`latexTag()`](https://dmurdoch.github.io/parseLatex/reference/names.md)
  function identifying the type of item. In this package the possible
  tags are

| Tag         | Description                                     | Type      |
|:------------|:------------------------------------------------|:----------|
| BLOCK       | A block enclosed in curly braces                | list      |
| COMMENT     | A LaTeX comment                                 | character |
| DISPLAYMATH | A display math block                            | list      |
| ENVIRONMENT | A LaTeX environment                             | list      |
| MACRO       | A LaTeX macro                                   | character |
| MATH        | An inline math block                            | list      |
| SPECIAL     | A non-alphabetic character                      | character |
| TEXT        | Text (consisting of letters only)               | character |
| VERB        | A verbatim environment                          | character |
| DEFINITION  | A command or environment definition             | list      |
| ERROR       | A block of items referenced in an error message | list      |

- The [`tools::parseLatex()`](https://rdrr.io/r/tools/parseLatex.html)
  parser does not have the `SPECIAL`; such characters are included in
  `TEXT`. It also doesn’t have the `DEFINITION` or `ERROR` tags.
  Definitions are treated as regular macros, which sometimes leads to
  parsing errors. Errors are always fatal. (`DEFINITION` will be added
  to R 4.5.0.)
- This parser stops when it reaches `\end{document}`, just as LaTeX
  does. The
  [`tools::parseLatex()`](https://rdrr.io/r/tools/parseLatex.html)
  parser in R 4.4.2 continues parsing beyond that, often leading to
  parsing errors as it tries to parse things that LaTeX would ignore.
  This will be fixed in R 4.5.0.
- In both implementations, some items (`COMMENT`, `MACRO`, `SPECIAL`,
  `TEXT`, and `VERB`) are stored as length 1 character vectors; the
  others are stored as lists of items corresponding to their content.  
- The list storage is different between the two. The
  [`tools::parseLatex()`](https://rdrr.io/r/tools/parseLatex.html)
  function stores some lists in two levels (e.g. the content of an
  environment named `item` would be in `item[[2]]`), while in this
  package, all lists contain the content directly (e.g. the content of
  that environment would be in `item` itself).
- This package marks all items with class `"LaTeX2item"`.
  [`tools::parseLatex()`](https://rdrr.io/r/tools/parseLatex.html) does
  not assign a class to items.
- This package provides print methods for class `"LaTeX2item"` so that
  individual items print nicely.
- Parsing errors are reported more informatively by this package.
- This parser supports “magic comments”. See the next section for
  details.
- This parser is more flexible in handling `verb` macros like `\Sexpr`.
  The [`tools::parseLatex()`](https://rdrr.io/r/tools/parseLatex.html)
  parser assumed there would be no braces within the macro (which is the
  case for legal [`Sweave()`](https://rdrr.io/r/utils/Sweave.html)
  source). This parser assumes any braces within the macro are balanced,
  e.g. this would be legal:  
    
  `\Sexpr{1 + {x <- 2; x + 1}}`  
    
  whereas unbalanced braces would not be.

As mentioned above,
[`parseLatex()`](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
does a little bit more than parsing. Both versions recognize LaTeX
environments and verbatim code.

The parser in this package also takes special action when it sees the
`document` environment: it stops parsing at `\end{document}`. (You can
use the
[`get_leftovers()`](https://dmurdoch.github.io/parseLatex/reference/get_leftovers.md)
function to see what parts of the input were skipped.)

It also changes the rules a bit when it sees macros defining things:
`\newenvironment`, `\renewenvironment`, `\newcommand`, `\renewcommand`
and `\providecommand`. The arguments to these macros are parsed but not
interpreted, allowing definitions to parse without triggering a syntax
error. For example:

    \newenvironment{newenv}{\begin{oldenv}}{\end{oldenv}}

The `\begin{oldenv}` part of the definition shouldn’t be interpreted
here as the start of an `oldenv` environment, because `\end{oldenv}`
isn’t in the same [`{}`](https://rdrr.io/r/base/Paren.html) block.

Two plain TeX versions of these macros are `\def` and `\let`. They are
recognized and an attempt is made to handle them, but there’s some
really arcane syntax possible. If you use that, it probably won’t be
parsed properly. Stick with simple syntax like

    \def\bea{\begin{eqnarray*}}

and you should be okay. This will likely be added to R 4.5.0, with the
same limitations.

## Magic Comments

The
[`parseLatex::parseLatex()`](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
parser can parse most LaTeX inputs, but not all. To allow it to be used
on files that contain unsupported syntax, it allows “magic comments” to
be inserted to control its actions.

Several LaTeX editors support magic comments of the form `% !TEX ...`,
and those were the model for `parseLatex` magic comment support. There
are 4 magic comments supported in this parser:

- `% !parser off` This tells the parser to absorb all following text as
  part of the comment, so anything that would be classed as a parsing
  error is never seen.
- `% !parser on` This tells it to resume normal parsing.
- `% !parser verb [name]` This tells the parser to add the name to the
  list of macros holding verbatim text, i.e. the list given by the
  `verb` argument when
  [`parseLatex()`](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  was called. The name should include the backslash, e.g.  
    
  `% !parser verb \Sexpr`  
    
  would add the default verb macro.
- `% !parser defcmd [name]` does the same for commands like
  `\newcommand`.
- `% !parser defenv [name]` does it for commands like `\newenvironment`.
- `% !parser verbatim [name]` This tells the parser to add the name to
  the list of environments holding verbatim text, i.e. the list given by
  the `verbatim` argument. For example  
    
  `% !parser verbatim Sinput`  
    
  would add one of the default verbatim environments.

The parser is quite strict about the format of the magic comments. The
whitespace between parts of it must be spaces, not tabs, and nothing
else can appear in the comment after the magic text other than more
spaces.

## Work in progress!

This is a work in progress, so if you have a use for something like this
and need help, post an “issue” on the Github page:
<https://github.com/dmurdoch/parseLatex> .
