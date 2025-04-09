#' @rdname options
#' @name options
#' @title Find or modify macro or environment options
#' @param items A list of latex items.
#' @param start Start looking at `items[[start]]`.
#' `start` may be a path.
#' @param which Which options do you want?  Some
#' macros support more than one set.
#' @description
#' Many Latex environments and macros take optional parameters
#' wrapped in square brackets.  `find_bracket_options` finds those,
#' assuming they come immediately after the macro.
#'
#' @returns `find_bracket_options` returns a [LaTeX2range] object pointing to the options within `items` (including the
#' brackets).
#'
#' @export
find_bracket_options <- function(items, which = 1L, start = 1L) {
  startpath <- start[-length(start)]
  start <- start[length(start)]
  if (length(startpath) > 1)
    items <- items[[startpath]]
  else
    startpath <- NULL

  subset <- flatten_itemlists(items)
  if (start > length(subset))
    return(NULL)

  subset <- subset[start:length(subset)]
  first <- find_char(subset,
                      "[",
                       all = which > 1L,
                       path = FALSE)

  if (length(first) == 0 || which > 1L && length(first) < which)
    return(NULL)

  depth <- 0L
  for (i in start:max(first)) {
    # All items before the one we are matching must be
    # blocks, whitespace, or other bracket options
    item <- subset[[index_to_path(i, subset)]]
    if (depth > 0L && is_bracket(item, "]"))
      depth <- depth - 1L
    else {
      if (is_bracket(item, "[")) {
        if (depth == 0L) {
          which <- which - 1L
          if (which == 0L) {
            first <- i
            break
          }
        }
        depth <- depth + 1L
      } else if (!is_block(item) && !is_whitespace(item))
        return(NULL)
    }
  }

  if (start > 1L)
    first <- first + start - 1L

  first <- index_to_path(first, items)

  thepath <- first[-length(first)]
  first <- first[length(first)]

  if (length(thepath) > 0)
    items <- items[[thepath]]
  else
    thepath <- NULL

  if (first >= length(items))
    return(NULL)

  last <- find_char(items[(first + 1L):length(items)], "]", all = FALSE)
  if (length(last) < 1L)
    return(NULL)
  else
    last <- last + first

  LaTeX2range(c(startpath, thepath), first:last)
}

#' @rdname options
#'
#' @returns `bracket_options` returns a [LaTeX2] object containing
#' the specified options.
#' @examples
#' parsed <- parseLatex("\\section[a]{b}")
#' macro <- find_macro(parsed, "\\section")
#' bracket_options(parsed, start = macro + 1)
#'
#' @export
bracket_options <- function(items, which = 1L, start = 1L) {
  as_LaTeX2(get_range(items,
                      find_bracket_options(items, which, start)))
}

replace_range <- function(items, i, value) {
  path <- i$path
  i <- i$range
  if (length(path) > 0) {
    items0 <- items
    items <- items0[[path]]
  }
  attrs <- attributes(items)
  iold <- seq_along(items)
  items <- c(items[iold < min(i)],
             value,
             items[iold > max(i)])
  attributes(items) <- attrs
  if (length(path) > 0) {
    items0[[path]] <- items
    items0
  } else
    items
}

#' @rdname options
#' @param asis Should newlines be added around the
#' value?
#' @param value The content to be inserted into the cell.  This
#' can be a [LaTeX2] object, or a character string that will be
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
  # `range` will be the range of the existing option to be replaced,
  # or to NULL if there isn't one. .
  # `path` will point to the insertion location
  range <- find_bracket_options(items, which, start)
  if (length(range)) {
    items <- drop_items(items, range)
    path <- c(range$path, min(range$range))
  } else {
    while(which > 1L) {
      which <- which - 1L
      range <- find_bracket_options(items, which, start)
      if (length(range)) {
        path <- c(range$path, max(range$range) + 1L)
        break
      } else
        value <- c(as_LaTeX2("[]"), value)
    }
    if (!length(range))
      path <- c(start[-length(start)], start[length(start)])
  }
  insert_values(items, path, value)
}

#' @rdname options
#' @param path If `TRUE`, return a path rather than
#' an index, as with [find_general()],
#' @description
#' Some Latex environments and macros take optional parameters
#' wrapped in curly brackets (braces). `find_brace_options` finds those
#' if they immediately follow the environment or macro (and possibly
#' some bracketed options).
#' @returns `find_brace_options` returns the index
#' or path to the block containing the options.
#' @export
find_brace_options <- function(items, which = 1L, start = 1L, path = FALSE) {
  if (has_itemlist(items))
    stop("this function doesn't work with itemlists.")
  result <- find_block(items[start:length(items)],
                       all = which > 1L,
                       path = path)

  if (length(result) < which)
    return(NULL)

  if (which > 1L)
    result <- result[[which]]

  if (start > 1L)
    result[1] <- result[1] + start - 1L

  result
}

#' @rdname options
#'
#' @returns `brace_options` returns a [LaTeX2] object containing
#' the specified options.
#' @examples
#' brace_options(parsed, start = macro + 1)
#'
#' @export
brace_options <- function(items, which = 1, start = 1) {
  path <- find_brace_options(items, which, start, path = TRUE)
  if (length(path))
    as_LaTeX2(items[[path]])
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
      value <- new_block(value)
  }
  # Get the path to the existing option.  If it exists,
  # remove it, otherwise get the path to the last thing
  # before it, and increment the final entry.
  path <- find_brace_options(items, which, start, path = TRUE)
  if (length(path))
    items <- drop_items(items, LaTeX2range(path, NULL))
  else {
    while(which > 1L) {
      which <- which - 1L
      path <- find_brace_options(items, which, start, path = TRUE)
      if (length(path)) {
        path[length(path)] <- path[length(path)] + 1L
        break
      } else
        value <- c(as_LaTeX2("{}"), value)
    }
    if (!length(path)) {  # we have no brace options, find
                          # the last bracket option
      repeat {
        range <- find_bracket_options(items, start = start)
        if (length(range))
          path <- c(range$path, max(range$range) + 1L)
        else
          break
      }
    }
  }
  insert_values(items, path, value)
}
