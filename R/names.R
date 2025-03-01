#' Utility functions finding names and types of objects
#' @name names
#' @param item A single latex item.
#'
#' @returns `latexTag()` returns the [LaTeX2] tag for the item or `NULL`.
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
    attr(item, "envname")
}

#' @rdname names
#'
#' @param item A [LaTeX2item] which is an environment
#' @param value A character string to set as the name
#' @export
`envName<-` <- function(item, value) {
  if (latexTag(item) != "ENVIRONMENT")
    stop("item is not an environment")
  attr(item, "envname") <- value
  item
}

#' @rdname names
#'
#' @returns `macroName()` returns the Latex macro, or `NULL`.
#' @export
macroName <- function(item) {
  if (latexTag(item) == "MACRO")
    as.character(item)
}
