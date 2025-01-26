#' Utility functions finding names and types of objects
#' @rdname names
#' @name Names
#' @param item A single latex item.
#'
#' @returns `latexTag()` returns the LaTeX2 tag for the item or `NULL`.
#' @export
latexTag <- function(item) {
  attr(item, "latex_tag")
}

#' @rdname names
#'
#' @returns `catcode()` returns the TeX catcode for the item, or `NULL`.
#' @export
catcode <- function(item) {
  attr(item, "catcode")
}

#' @rdname names
#'
#' @returns `envName()` returns the Latex environment name for an item, or `NULL`.
#' @export
envName <- function(item) {
  if (latexTag(item) == "ENVIRONMENT")
    item[[1]]
}

#' @rdname names
#'
#' @returns `macroName()` returns the Latex macro, or `NULL`.
#' @export
macroName <- function(item) {
  if (latexTag(item) == "MACRO")
    as.character(item)
}
