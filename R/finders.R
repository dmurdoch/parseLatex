
#' @rdname finders
#' @title Miscellaneous low-level finders
#' @param items A list of latex items.
#' @returns `find_whitespace()` returns the indices of
#' whitespace in `items`.
#' @export
find_whitespace <- function(items) {
  seq_along(items)[sapply(items, is_whitespace)]
}


#' @rdname finders
#' @param envtypes Which types of environment to look for.
#' @returns `find_env()` returns the indices within `items`
#' of environments in `envtypes`.
#' @export
find_env <- function(items, envtypes) {
  which(sapply(seq_along(items),
               function(i)
                 is_env(items[[i]]) &&
                 envName(items[[i]]) %in% envtypes))
}

#' @rdname finders
#' @param macros Which types of macros to look for.
#' @returns `find_macro()` returns the index within `items`
#' of instances in `macros`.
#' @export
find_macro <- function(items, macros) {
  which(sapply(seq_along(items),
               function(i)
                 is_macro(items[[i]]) &&
                 macroName(items[[i]]) %in% macros))
}

#' @rdname finders
#' @param codes Which codes to look for.
#' @returns `find_catcode()` returns the index within `items`.
#' of specials matching `code`.
#' @export
find_catcode <- function(items, codes) {
  which(sapply(seq_along(items),
               function(i)
                 latexTag(items[[i]]) == "SPECIAL" &&
                 catcode(items[[i]]) %in% codes))
}

#' @rdname finders
#' @param char Which character to look for.
#' @returns `find_char()` returns the index within `items`
#' of characters matching `char`.  Only characters
#' marked as SPECIAL by the parser will be found.
#' @export
find_char <- function(items, char) {
  which(sapply(seq_along(items),
               function(i)
                 latexTag(items[[i]]) == "SPECIAL" &&
                 items[[i]] == char))
}

