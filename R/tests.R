#' @title Test objects
#' @rdname tests
#' @name tests

#' @param item An object of class "LaTeX2item" to test.
#' @param envtypes Types of Latex environment to check for,
#' e.g. `"table"`.
#'
#' @returns `is_env()` returns a boolean if the item matches.
#' @export
#'
is_env <- function(item,  envtypes = NULL) {
  inherits(item, "LaTeX2item") &&
  !is.null(envName(item)) &&
    (is.null(envtypes) || item[[1]] %in% envtypes)
}

#' @rdname tests
#' @param macros Which macros to match, e.g. `"\\\\caption"`.
#'
#' @returns `is_macro()` returns a boolean indicating the match.
#' @export
is_macro <- function(item, macros = NULL) {
  inherits(item, "LaTeX2item") &&
  !is.null(macroName(item)) &&
    (is.null(macros) || item %in% macros)
}

#' @rdname tests

#' @returns `is_block()` returns a boolean indicating whether the `item` is a block wrapped in curly braces.
#' @export
is_block <- function(item) {
  inherits(item, "LaTeX2item") &&
  latexTag(item) == "BLOCK"
}

#' @rdname tests
#' @param bracket Which bracket are we looking for?
#'
#' @returns `is_bracket()` returns a boolean indicating that the `item` is a bracket of the
#' specified type.
#' @export
#'
#' @examples
#' is_bracket(parseLatex("[]")[[1]], "[")
is_bracket <- function(item, bracket) {
  inherits(item, "LaTeX2item") &&
  latexTag(item) == "SPECIAL" &&
    catcode(item) == OTHER &&
    item == bracket
}

#' @rdname tests

#' @returns `is_whitespace()` returns a boolean indicating if the
#' `item` is a space, tab or newline.
#' @export
is_whitespace <- function(item) {
  if (inherits(item, "LaTeX2item")) {
    cat <- catcode(item)
    !is.null(cat) && cat %in% c(NEWLINE, SPACE)
  } else
    FALSE
}
