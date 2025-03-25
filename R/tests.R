#' @title Test objects
#' @rdname tests
#' @name tests

#' @param item An object of class [LaTeX2item] to test.
#' @param envtypes Types of Latex environment to check for,
#' e.g. `"table"`.
#'
#' @returns `is_env()` returns a boolean if the item matches.
#' @export
#'
is_env <- function(item,  envtypes = NULL) {
  inherits(item, "LaTeX2item") &&
  !is.null(envName(item)) &&
    (is.null(envtypes) || envName(item) %in% envtypes)
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

#' @rdname tests

#' @returns `is_text()` returns a boolean indicating if the
#' `item` is text.
#' @export
is_text <- function(item) {
  inherits(item, "LaTeX2item") &&
      latexTag(item) == "TEXT"
}

#' @rdname tests

#' @returns `is_error()` returns a boolean indicating if the
#' `item` is an error.
#' @export
is_error <- function(item) {
  inherits(item, "LaTeX2item") &&
    latexTag(item) == "ERROR"
}

#' @rdname tests
#' @returns `is_itemlist()` returns a boolean indicating if the
#' `item` is an [ITEMLIST] item.
#' @export
is_itemlist <- function(item) {
  inherits(item, "LaTeX2item") &&
    latexTag(item) == "ITEMLIST"
}

#' @rdname tests
#' @returns `is_placeholder()` returns a boolean indicating if the
#' `item` is a [PLACEHOLDER] item.
#' @export
is_placeholder <- function(item) {
  inherits(item, "LaTeX2item") &&
    latexTag(item) == "PLACEHOLDER"
}

#' @rdname tests
#' @param char A character to match
#' @returns `is_char()` returns a boolean indicating if the
#' `item` is a `SPECIAL` matching `char`.
#' @export
is_char <- function(item, char)
  latexTag(item) == "SPECIAL" && item == char
