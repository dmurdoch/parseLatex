#' @rdname options
#' @title Find macro or environment options
#' @param items A list of latex items.
#' @param start Start looking at `items[[start]]`.
#' @param which Which options do you want?  Some
#' macros support more than one set.
#' @description
#' Many Latex environments and macros take optional parameters
#' wrapped in square brackets.  `find_bracket_options` finds those,
#' assuming they come immediately after the macro.
#'
#' @returns `find_bracket_options` returns indices into `items` of the options (including the
#' brackets).
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
#' @returns `bracket_options` returns a LaTeX2 object containing
#' the specified options.
#' @examples
#' parsed <- parseLatex("\\section[a]{b}")
#' macro <- find_macro(parsed, "\\section")
#' bracket_options(parsed, start = macro + 1)
#'
#' @export
bracket_options <- function(items, which = 1, start = 1) {
  as_LaTeX2(items[find_bracket_options(items, which, start)])
}

#' @rdname options
#' @param asis Should newlines be added around the
#' value?
#' @param value The content to be inserted into the cell.  This
#' can be a LaTeX2 object, or a character string that will be
#' converted to one.
#' @examples
#' bracket_options(parsed, start = macro + 1) <- "Short Title"
#' parsed
#'
#' @export
`bracket_options<-` <- function(items, which = 1, start = 1, asis = FALSE, value) {
  value <- as_LaTeX2(value)
  if (!asis) {
    if (!is_bracket(value[[1]], "["))
      value <- c(as_LaTeX2("["), value)
    if (!is_bracket(value[[length(value)]], "]"))
      value <- c(value, as_LaTeX2("]"))
  }
  i <- find_bracket_options(items, which, start)
  if (!length(i)){
    while(which > 1) {
      which <- which - 1
      i <- find_bracket_options(items, which, start)
      if (length(i)) {
        i <- max(i) + 0.5
        break
      } else
        value <- c(as_LaTeX2("[]"), value)
    }
    if (!length(i))
      i <- start + 0.5
  }
  old <- items
  iold <- seq_along(old)
  items <- c(old[iold < min(i)],
             value,
             old[iold > max(i)])
  as_LaTeX2(items)
}

#' @rdname options
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

#' @rdname options
#'
#' @returns `brace_options` returns a LaTeX2 object containing
#' the specified options.
#' @examples
#' brace_options(parsed, start = macro + 1)
#'
#' @export
brace_options <- function(items, which = 1, start = 1) {
  as_LaTeX2(items[find_brace_options(items, which, start)])
}

#' @rdname options
#' @examples
#' brace_options(parsed, start = macro + 1) <- "Long Title"
#' parsed
#'
#' @export
`brace_options<-` <- function(items, which = 1, start = 1, asis = FALSE, value) {
  value <- as_LaTeX2(value)
  if (!asis) {
    if (length(value) != 1 || !is_block(value[[1]]))
      value <- as_LaTeX2(paste0("{", deparseLatex(value), "}"))
  }
  i <- find_brace_options(items, which, start)
  if (!length(i)){
    while(which > 1) {
      which <- which - 1
      i <- find_brace_options(items, which, start)
      if (length(i)) {
        i <- max(i) + 0.5
        break
      } else
        value <- c(as_LaTeX2("{}"), value)
    }
  }
  if (!length(i)) {  # we have no brace options, find
                     # the last bracket option
    repeat {
      i <- find_bracket_options(items, start = start)
      if (length(i))
        start <- max(i) + 1
      else
        break
    }
    i <- start - 0.5
  }

  old <- items
  iold <- seq_along(old)
  items <- c(old[iold < min(i)],
             value,
             old[iold > max(i)])
  as_LaTeX2(items)
}
