# parseLatex 0.2.5

* Added `stdbool.h` to the includes for compatibility with older
`gcc`.  (Thanks to Dirk Eddelbuettel for the report and PR #2).
* Added a configure script to check for bison (on Unix-alike only).
* Adjacent math environments were treated as a syntax error.
* `\let` is now handled the same way as `\def`:  the next two
tokens are not considered for evaluation.

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
