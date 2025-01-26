#' @rdname options
#' @title Find macro or environment options
#' @param which Which bracket options do you want?  Some
#' macros support more than one set.
#'
#' @param start Start looking at `items[[start]]`
#' @description
#' Many Latex environments and macros take optional parameters
#' wrapped in square brackets.  `find_bracket_options` finds those,
#' assuming they come immediately after the macro.
#'
#' @returns `find_bracket_options` returns indices into `items` of the options (including the
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

#' @rdname options
#'
#' @param items A list of latex items
#' @param which Return this block
#' @param start Start looking at `items[[start]]`
#'
#' @description
#' Some Latex environments and macros take optional parameters
#' wrapped in curly brackets (braces). `find_brace_options` finds those
#' if they immediately follow the environment or macro (and possibly
#' some bracketed options).
#' @returns `find_brace_options` returns the index of the block containing the options.
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
