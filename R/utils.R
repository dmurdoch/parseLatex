#' @title Miscellaneous utilities
#' @rdname Utilities
#' @name utils

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
drop_whitespace <- function(items)
  drop_items(items, find_whitespace(items))

#' @rdname Utilities
#'
#' @returns `include_whitespace()` returns `which` with
#' following whitespace (blanks, tabs, newlines) included.
#' @export
include_whitespace <- function(items, which) {
  whitespace <- find_whitespace(items)
  repeat {
    following <- setdiff(intersect(whitespace, which + 1),
                         which)
    if (!length(following))
      return(sort(which))
    which <- c(which, following)
  }
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
#' @param splits Which items divide the parts?
#' @returns `split_list()` returns a list of pieces
#' separated at the splits
#' @export
split_list <- function(items, splits) {
  prev <- 0
  result <- lapply(splits, function(s) {
    if (s > prev + 1) {
      sel <- items[(prev + 1):(s - 1)]
      prev <<- s
      sel
    } else
      items[FALSE]
  })
  # Add in one more after the last split
  extras <- items[seq_along(items) > max(splits)]
  result <- c(result, list(extras))
  result
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
