#' @title Coerce to LaTeX2

#' @param x An object to convert to a LaTeX2 object.
#' @returns `as_LaTeX2()` converts `x` to a LaTeX2 object.
#' @export
as_LaTeX2 <- function(x) {
  if (inherits(x, "LaTeX2"))
    x
  else if (inherits(x, "LaTex2item"))
    structure(list(x), class = "LaTeX2")
  else if (is.list(x))
    structure(x, class = "LaTeX2")
  else if (is.character(x))
    parseLatex(x)
}
