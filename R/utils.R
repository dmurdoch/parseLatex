#' Utility functions for working with parsed Latex.
#' @rdname Utilities
#' @param item A single latex item.
#'
#' @returns `latexTag()` returns the LaTeX2 tag for the item or `NULL`.
#' @export
latexTag <- function(item) {
  attr(item, "latex_tag")
}

#' @rdname Utilities
#'
#' @returns `catcode()` returns the TeX catcode for the item, or `NULL`.
#' @export
catcode <- function(item) {
  attr(item, "catcode")
}

#' @rdname Utilities
#'
#' @returns `envname()` returns the Latex environment name for an item, or `NULL`.
#' @export
envname <- function(item) {
  if (latexTag(item) == "ENVIRONMENT")
    item[[1]]
}

#' @rdname Utilities
#'
#' @returns `macroname()` returns the Latex macro, or `NULL`.
#' @export
macroname <- function(item) {
  if (latexTag(item) == "MACRO")
    as.character(item)
}

is_env <- function(item,  types = NULL) {
  !is.null(envname(item)) &&
    (is.null(types) || item[[1]] %in% types)
}

is_macro <- function(item, types = NULL) {
  !is.null(macroname(item)) &&
    (is.null(types) || item %in% types)
}

is_block <- function(item) {
  latexTag(item) == "BLOCK"
}

#' @rdname Utilities
#' @param bracket Which bracket are we looking for?
#'
#' @returns `is_bracket()` returns a boolean indicating that the `item` is a bracket of the
#' specified type.
#' @export
#'
#' @examples
#' is_bracket(parseLatex("[]")[[1]], "[")
is_bracket <- function(item, bracket) {
  latexTag(item) == "SPECIAL" &&
    catcode(item) == 12 &&
    item == bracket
}

isWhitespace <- function(item) {
  cat <- catcode(item)
  !is.null(cat) && cat %in% c(5, 10)
}

#' @rdname Utilities
#'
#' @returns `dropWhitespace()` returns the list of items with whitespace (blanks, tabs, newlines) removed.
#' @export
dropWhitespace <- function(items) {
  structure(Filter(function(item) !isWhitespace(item), items), class = "LaTeX2")
}

#' Extract bracket wrapped options from Latex code.
#'
#' @param items A list of latex items
#'
#' @param which Which bracket options do you want?  Some
#' macros support more than one set.
#'
#' @param drop Should whitespace be dropped?
#'
#' @description
#' Many Latex environments and macros take optional parameters
#' wrapped in square brackets.  This function extracts those,
#' assuming they come immediately after the macro.
#'
#' @export
bracket_options <- function(items, which = 1, drop = TRUE) {
  start <- NA
  n <- length(items)
  for (i in seq_len(n)) {
    if (is.na(start) && is_bracket(items[[i]], "[")) {
      which <- which - 1
      start <- i
    } else if (!is.na(start) && is_bracket(items[[i]], "]")) {
      if (which == 0) {
        result <- items[start + seq_len(i - start - 1)]
        if (drop)
          result <- dropWhitespace(result)
        return(structure(result, class = "LaTeX2"))
      } else
        start <- NA
    } else if (is.na(start) && !isWhitespace(items[[i]]))
      break
  }
  invisible()
}

#' Extract brace wrapped options from Latex code.
#'
#' @param items A list of latex items
#' @param which Return this block
#' @param drop Should whitespace be dropped from the result?
#'
#' @description
#' Some Latex environments and macros take optional parameters
#' wrapped in curly brackets (braces).  This function extracts those
#' if they immediately follow the environment or macro and possibly
#' some bracketed options.
#'
#' @export
brace_options <- function(items, which = 1, drop = TRUE) {
  items <- dropWhitespace(items)
  while (length(items)) {
    if (is_bracket(items[[1]], "[")) {
      skip <- length(bracket_options(items, drop = FALSE)) + 2
      items <- items[skip + seq_len(length(items) - skip)]
    } else if (is_block(items[[1]])) {
      which <- which - 1
      if (which == 0) {
        result <- items[[1]]
        if (drop)
          result <- dropWhitespace(result)
        return(structure(result, class = "LaTeX2"))
      } else
        items <- items[-1]
    } else
      break
  }
  invisible()
}

tabular_type <- function(item, tabtypes = c("tabular", "longtable")) {
  if (is_env(item, tabtypes))
    item[[1]]
}


