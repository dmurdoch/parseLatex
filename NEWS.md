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

# parseLatex 0.1.0

* Initial version.
