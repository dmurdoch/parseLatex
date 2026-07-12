# Changelog

## parseLatex 0.4.2

- Improved support for syntax of `\let` and `\def`.

## parseLatex 0.4.1

CRAN release: 2025-06-06

- Added
  [`row_to_vector()`](https://dmurdoch.github.io/parseLatex/reference/vector_to_row.md).
- Added
  [`is_text()`](https://dmurdoch.github.io/parseLatex/reference/tests.md)
  and
  [`is_error()`](https://dmurdoch.github.io/parseLatex/reference/tests.md).
- Added
  [`showErrors()`](https://dmurdoch.github.io/parseLatex/reference/showErrors.md),
  and changed the formatting of errors in
  [`deparseLatex()`](https://dmurdoch.github.io/parseLatex/reference/deparseLatex.md).
  `parseLatex(recover = TRUE)` now calls
  [`showErrors()`](https://dmurdoch.github.io/parseLatex/reference/showErrors.md)
  rather than generating warnings.
- Fixed some parsing bugs related to error recovery.
- Sequences of spaces and tabs and sequences of linebreaks are each
  collected into single items containing the full sequence of
  characters.
- Deparsing of `SPECIAL`s has been improved.
- Added
  [`trim_whitespace()`](https://dmurdoch.github.io/parseLatex/reference/Utilities.md),
  `envName()<-`,
  [`find_block()`](https://dmurdoch.github.io/parseLatex/reference/finders.md)
  and
  [`insert_values()`](https://dmurdoch.github.io/parseLatex/reference/path_to.md)
  to help with `kableExtra` support.
- Added support for `tabu` tables.
- Added
  [`is_char()`](https://dmurdoch.github.io/parseLatex/reference/tests.md),
  [`find_caption()`](https://dmurdoch.github.io/parseLatex/reference/find_caption.md),
  [`drop_caption()`](https://dmurdoch.github.io/parseLatex/reference/find_caption.md),
  [`path_to_caption()`](https://dmurdoch.github.io/parseLatex/reference/find_caption.md),
  [`get_range()`](https://dmurdoch.github.io/parseLatex/reference/set_range.md).
- [`drop_items()`](https://dmurdoch.github.io/parseLatex/reference/Utilities.md)
  now works on `LaTeX2Item` lists.
- [`tableRow()`](https://dmurdoch.github.io/parseLatex/reference/tableRow.md)
  by default ignores `\pagebreak` and `\nopagebreak`. The new argument
  `withExtras` allows it to include these and other extras before the
  line in the result. `withData` allows it to exclude the data.
  [`find_tableRow()`](https://dmurdoch.github.io/parseLatex/reference/tableRow.md)
  and `tableRow<-` have similar modifications.
- [`find_rule()`](https://dmurdoch.github.io/parseLatex/reference/tableRule.md)
  and
  [`rule()`](https://dmurdoch.github.io/parseLatex/reference/tableRule.md)
  now remove the final whitespace from the result.
- Added
  [`columnOption()`](https://dmurdoch.github.io/parseLatex/reference/tableOption.md)
  and `columnOption<-()` functions.
- Added
  [`new_env()`](https://dmurdoch.github.io/parseLatex/reference/Utilities.md),
  similar to
  [`new_block()`](https://dmurdoch.github.io/parseLatex/reference/Utilities.md).
- Added
  [`split_chars()`](https://dmurdoch.github.io/parseLatex/reference/Utilities.md),
  usually used to split text or whitespace into individual characters.
- Constants for the TeX catcodes are now exported, e.g. `NEWLINE`.
- The `find_*` functions now have an `all` argument, to allow the search
  to be stopped at the first success, and a `path` argument, to control
  the form of the return value.
- Added `ITEMLIST` and `PLACEHOLDER` LaTeX tag values. These are used
  internally to organize long lists of items, e.g. to allow the rows of
  a table to be indexed directly.
- Fixed bug in
  [`parseLatex()`](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  in handling `$` within a definition.

## parseLatex 0.3.0

CRAN release: 2025-02-20

- Added `stdbool.h` to the includes for compatibility with older `gcc`.
  (Thanks to Dirk Eddelbuettel for the report and PR
  [\#2](https://github.com/dmurdoch/parseLatex/issues/2)).
- Added a configure script to check for bison (on Unix-alike only).
- Adjacent math environments were treated as a syntax error.
- `\let` is now handled the same way as `\def`: the next two tokens are
  not considered for evaluation.
- Added arguments `defcmd` and `defenv` to
  [`parseLatex()`](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  to allow the user to specify defining macros. These names are also now
  allowed in the magic comments.
- Spaces between a `verb` macro and its argument caused the parsing to
  be incorrect.
- Added argument `recover` to
  [`parseLatex()`](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md),
  to attempt to recover after a parsing error. This may help with
  diagnosing the error. The error will be wrapped in an `ERROR` tag
  which is displayed as `>>>text causing error<<<`.

## parseLatex 0.2.1

CRAN release: 2025-02-11

- Minor modifications for CRAN submission.

## parseLatex 0.2.0

- Added
  [`find_sequence()`](https://dmurdoch.github.io/parseLatex/reference/find_sequence.md),
  [`set_contents()`](https://dmurdoch.github.io/parseLatex/reference/get_contents.md).
- Add `DEFINITION` tag to allow `\begin` etc. to be temporarily ignored.
  This allows parseLatex to parse `\newenvironment` and related macros
  that create definitions.
- [`parseLatex()`](https://dmurdoch.github.io/parseLatex/reference/parseLatex_fn.md)
  now stops parsing after `\end{document}` just as LaTeX does. The
  [`get_leftovers()`](https://dmurdoch.github.io/parseLatex/reference/get_leftovers.md)
  function can retrieve the unparsed text.
- Added support for “magic comments”. See the vignette for details.
- `verb` macros now allow embedded braces as long as they are balanced.

## parseLatex 0.1.0

- Initial version.
