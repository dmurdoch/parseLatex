#' @title Coerce to LaTeX2

#' @param x An object to convert to a [LaTeX2] object.
#' @returns `as_LaTeX2()` converts `x` to a [LaTeX2] object.
#' @export
as_LaTeX2 <- function(x) {
  if (inherits(x, "LaTeX2")) {
    x
  } else if (inherits(x, "LaTeX2item")) {
    structure(list(x), class = "LaTeX2")
  } else if (is.list(x)) {
    structure(x, class = "LaTeX2")
  } else {
    parseLatex(as.character(x))
  }
}

#' @rdname as_LaTeX2
#' @param ... Objects to concatenate.
#' @returns `latex2()` converts the arguments to [LaTeX2] objects
#' and concatenates them into a new [LaTeX2] object.
#' @export
latex2 <- function(...)
  as_LaTeX2(do.call(c, lapply(list(...), as_LaTeX2)))
