#' Utility functions for working with parsed Latex.
#' @rdname Utilities
#' @name Utilities
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
#' @returns `envName()` returns the Latex environment name for an item, or `NULL`.
#' @export
envName <- function(item) {
  if (latexTag(item) == "ENVIRONMENT")
    item[[1]]
}

#' @rdname Utilities
#'
#' @returns `macroName()` returns the Latex macro, or `NULL`.
#' @export
macroName <- function(item) {
  if (latexTag(item) == "MACRO")
    as.character(item)
}

#' @rdname Utilities
#' @param envtypes Types of Latex environment to check for,
#' e.g. `"table"`.
#'
#' @returns `is_env()` returns a boolean if the item matches.
#' @export
#'
is_env <- function(item,  envtypes = NULL) {
  !is.null(envName(item)) &&
    (is.null(envtypes) || item[[1]] %in% envtypes)
}

#' @rdname Utilities
#' @param macros Which macros to match, e.g. `"\\\\caption"`.
#'
#' @returns `is_macro()` returns a boolean indicating the match.
#' @export
is_macro <- function(item, macros = NULL) {
  !is.null(macroName(item)) &&
    (is.null(macros) || item %in% macros)
}

#' @rdname Utilities

#' @returns `is_block()` returns a boolean indicating whether the `item` is a block wrapped in curly braces.
#' @export
is_block <- function(item) {
  latexTag(item) == "BLOCK"
}

#' @rdname Utilities
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

#' @rdname Utilities

#' @returns `is_whitespace()` returns a boolean indicating if the
#' `item` is a space, tab or newline.
#' @export
is_whitespace <- function(item) {
  cat <- catcode(item)
  !is.null(cat) && cat %in% c(5, 10)
}

#' @rdname Utilities
#' @param items A LaTeX2 object or list of items.
#' @param which Which items to operate on.
#' @returns `drop_items()` returns the list of items with specific items removed.
#' @export
drop_items <- function(items, which) {
  if (length(which))
    items <- items[-which]
  as_LaTeX2(items)
}

#' @rdname Utilities
#'
#' @returns `select_items()` returns the list of subsetted items
#' @export
select_items <- function(items, which) {
  as_LaTeX2(items[which])
}

#' @rdname Utilities
#'
#' @returns `drop_whitespace()` returns the list of items with whitespace (blanks, tabs, newlines) removed.
#' @export
drop_whitespace <- function(items) {
  as_LaTeX2(Filter(function(item) !is_whitespace(item), items))
}

#' Find bracket wrapped options from Latex code.
#'
#' @param items A list of latex items
#'
#' @param which Which bracket options do you want?  Some
#' macros support more than one set.
#'
#' @param start Start looking at `items[[start]]`
#' @description
#' Many Latex environments and macros take optional parameters
#' wrapped in square brackets.  This function finds those,
#' assuming they come immediately after the macro.
#'
#' @returns Indices into `items` of the options (including the
#' brackets)
#'
#' @export
find_bracket_options <- function(items, which = 1, start = 1) {
  begin <- NA
  n <- length(items)
  if (start <= n)
    for (i in start:n) {
      if (is.na(begin) && is_bracket(items[[i]], "[")) {
        which <- which - 1
        begin <- i
      } else if (!is.na(begin) && is_bracket(items[[i]], "]")) {
        if (which == 0) {
          result <- seq.int(begin, i)
          return(result)
        } else
          begin <- NA
      } else if (is.na(begin) && !is_whitespace(items[[i]]))
        break
    }
  invisible()
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
#' @param start Start looking at `items[[start]]`
#' @description
#' Many Latex environments and macros take optional parameters
#' wrapped in square brackets.  This function extracts those,
#' assuming they come immediately after the macro.
#'
#' @export
bracket_options <- function(items, which = 1, drop = TRUE, start = 1) {
  i <- find_bracket_options(items, which = which, start = start)
  i <- i[-c(1, length(i))]
  if (drop)
    drop_whitespace(items[i])
  else
    items[i]
}

#' Find brace wrapped options in Latex code.
#'
#' @param items A list of latex items
#' @param which Return this block
#' @param start Start looking at `items[[start]]`
#'
#' @description
#' Some Latex environments and macros take optional parameters
#' wrapped in curly brackets (braces).  This function finds those
#' if they immediately follow the environment or macro and possibly
#' some bracketed options.
#' @returns The index of the block containing the options.
#' @export
find_brace_options <- function(items, which = 1, start = 1) {
  n <- length(items)
  i <- start
  while (i <= n) {
    if (is_block(items[[i]])) {
      which <- which - 1
      if (which == 0)
        return(i)
    } else if (is_bracket(items[[i]], "["))
      i <- max(find_bracket_options(items, start = i))
    else if (!is_whitespace(items[[i]]))
      return(invisible())
    i <- i + 1
  }
  invisible()
}

#' Extract brace wrapped options from Latex code.
#'
#' @param items A list of latex items
#' @param which Return this block
#' @param drop Should whitespace be dropped from the result?
#' @param start Start looking at `items[[start]]`
#'
#' @description
#' Some Latex environments and macros take optional parameters
#' wrapped in curly brackets (braces).  This function extracts those
#' if they immediately follow the environment or macro and possibly
#' some bracketed options.
#'
#' @export
brace_options <- function(items, which = 1, drop = TRUE, start = 1) {
  i <- find_brace_options(items, which = which, start = start)
  if (drop)
    drop_whitespace(items[[c(i, 1)]])
  else
    items[[c(i, 1)]]
}

#' @rdname Utilities

#' @returns `find_env()` returns the indices within `items`
#' of environments in `envtypes`.
#' @export
find_env <- function(items, envtypes) {
  which(sapply(seq_along(items),
               function(i)
                 is_env(items[[i]]) &&
                 envName(items[[i]]) %in% envtypes))
}

#' @rdname Utilities

#' @returns `find_macro()` returns the index within `items`
#' of instances in `macros`.
#' @export
find_macro <- function(items, macros) {
  which(sapply(seq_along(items),
               function(i)
                 is_macro(items[[i]]) &&
                 macroName(items[[i]]) %in% macros))
}

#' @rdname Utilities
#' @param codes Which codes to look for
#' @returns `find_catcode()` returns the index within `items`
#' of specials matching `code`.
#' @export
find_catcode <- function(items, codes) {
  which(sapply(seq_along(items),
               function(i)
                 latexTag(items[[i]]) == "SPECIAL" &&
                 catcode(items[[i]]) %in% codes))
}

#' @rdname Utilities
#' @param splits Which items divide the parts?
#' @returns `split_list()` returns a list of pieces
#' separated at the splits
#' @export
split_list <- function(items, splits) {
  prev <- 0
  lapply(splits, function(s) {
    if (s > prev + 1) {
      sel <- items[(prev + 1):(s - 1)]
      prev <<- s
      sel
    } else
      list()
  })
}

#' @rdname Utilities
#' @param splits Which items divide the parts?
#' @returns `split_latex()` returns a list of pieces
#' separated at the splits
#' @export
split_latex <- function(items, splits) {
  lapply(split_list(items, splits), as_LaTeX2)
}

#' Convenience function to get contents from an item
#'
#' @param item An item from a Latex list (or a list with one item)
#'
#' @returns The contents of the item as a Latex list, or as a
#' character string.
#' @export
#' @examples
#' get_contents(parseLatex("{abc}"))
#'
get_contents <- function(item) {
  tag <- latexTag(item)
  if (is.null(tag) && is.list(item)) {
    if (length(item) > 1)
      warning("only using the first element")
    item <- item[[1]]
    tag <- latexTag(item)
  }
  if (is.null(tag))
    NULL
  else if (tag == "ENVIRONMENT")
    as_LaTeX2(item[[2]])
  else if (tag == "BLOCK")
    as_LaTeX2(item)
  else
    as.character(item)
}
