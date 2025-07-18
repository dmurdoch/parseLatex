# parseLatex 0.4.2

* Improved support for syntax of `\let` and `\def`.

# parseLatex 0.4.1

* Added `row_to_vector()`.
* Added `is_text()` and `is_error()`.
* Added `showErrors()`, and changed the formatting of 
  errors in `deparseLatex()`.  `parseLatex(recover = TRUE)` now calls
  `showErrors()` rather than generating warnings.
* Fixed some parsing bugs related to error recovery.
* Sequences of spaces and tabs and sequences of linebreaks are
  each collected into single items containing the full sequence of
  characters.
* Deparsing of `SPECIAL`s has been improved.
* Added `trim_whitespace()`, `envName()<-`, `find_block()` and `insert_values()`
  to help with `kableExtra` support.
* Added support for `tabu` tables.
* Added `is_char()`, `find_caption()`, `drop_caption()`, `path_to_caption()`,
  `get_range()`.
* `drop_items()` now works on `LaTeX2Item` lists.
* `tableRow()` by default ignores `\pagebreak` and `\nopagebreak`. 
The new argument `withExtras` allows it to include these and other 
extras before the line in the result.  `withData` allows it to exclude
the data.  `find_tableRow()` and `tableRow<-`
have similar modifications.
* `find_rule()` and `rule()` now remove the final whitespace from
the result.
* Added `columnOption()` and `columnOption<-()` functions.
* Added `new_env()`, similar to `new_block()`.
* Added `split_chars()`, usually used to split text or whitespace into individual
characters.
* Constants for the TeX catcodes are now exported, e.g. `NEWLINE`.
* The `find_*` functions now have an `all` argument, to allow
the search to be stopped at the first success, and a `path` argument,
to control the form of the return value.
* Added `ITEMLIST` and `PLACEHOLDER` LaTeX tag values.  These are 
used internally to organize long lists of items, e.g. to allow
the rows of a table to be indexed directly.
* Fixed bug in `parseLatex()` in handling `$` within a definition.

# parseLatex 0.3.0

* Added `stdbool.h` to the includes for compatibility with older
`gcc`.  (Thanks to Dirk Eddelbuettel for the report and PR #2).
* Added a configure script to check for bison (on Unix-alike only).
* Adjacent math environments were treated as a syntax error.
* `\let` is now handled the same way as `\def`:  the next two
tokens are not considered for evaluation.
* Added arguments `defcmd` and `defenv` to `parseLatex()` to
allow the user to specify defining macros.  These names
are also now allowed in the magic comments.
* Spaces between a `verb` macro and its argument caused the parsing
to be incorrect.
* Added argument `recover` to `parseLatex()`, to attempt to recover
after a parsing error.  This may help with diagnosing the error.
The error will be wrapped in an `ERROR` tag which is displayed
as `>>>text causing error<<<`.

# parseLatex 0.2.1

* Minor modifications for CRAN submission.

# parseLatex 0.2.0

* Added `find_sequence()`, `set_contents()`.
* Add `DEFINITION` tag to allow `\begin` etc. to be temporarily
ignored.  This allows parseLatex to parse `\newenvironment` and
related macros that create definitions.
* `parseLatex()` now stops parsing after `\end{document}` just
as LaTeX does.  The `get_leftovers()` function can retrieve
the unparsed text.
* Added support for "magic comments".  See the vignette
for details.
* `verb` macros now allow embedded braces as long as
they are balanced.

# parseLatex 0.1.0

* Initial version.
