zap_srcref <- function(items) {
  attr(items, "srcref") <- NULL
  if (is.list(items))
    items <- lapply(items, zap_srcref)
  items
}

zap_whitespace <- function(items) {
  if (is.list(items)) {
    origidx <- seq_along(items)
    whitespace <- find_whitespace(items)
    if (length(whitespace)) {
      origidx <- setdiff(origidx, whitespace)
      items <- drop_items(items, whitespace)
    }
    items <- lapply(items, zap_whitespace)
    structure(items, origidx = origidx)
  } else
    items
}

#' Find a code sequence
#'
#' @param items,sequence [LaTeX2] objects or lists.
#' @param all Whether to return all matches, or just the first.
#' @param ignore_whitespace Whether to ignore whitespace in comparisons.
#' @returns `find_sequence()` returns a path or list of paths where `sequence`
#' occurs within `items`.
#' @export
#' @examples
#' find_sequence(parseLatex("a & b & c"), "b & c")
find_sequence <- function(items, sequence, all = FALSE,
                          ignore_whitespace = TRUE) {
  items <- as_LaTeX2(items)
  sequence <- as_LaTeX2(sequence)
  if (ignore_whitespace) {
    items <- zap_whitespace(items)
    sequence <- zap_whitespace(sequence)
  }

  result <- NULL
  lendiff <- length(items) - length(sequence)
  if (lendiff >= 0) {
    idx <- seq_along(sequence) - 1
    for (i in seq_len(lendiff + 1)) {
      range <- i + idx
      if (items_are_equal(as_LaTeX2(items[range]), sequence)) {
        if (ignore_whitespace)
          range <- attr(items, "origidx")[range]
        res <- LaTeX2range(numeric(), range)
        if (!all)
          return(res)
        else
          result <- c(result, list(res))
      }
    }
  }
  for (i in seq_along(items)) {
    if (is.list(items[[i]])) {
      res <- find_sequence(items[[i]], sequence, all = all,
                           ignore_whitespace = TRUE)
      if (length(res)) {
        if (ignore_whitespace)
          j <- attr(items, "origidx")[i]
        else
          j <- i
        if (!all)
          return(LaTeX2range(c(j, res$path), res$range))
        else
          result <- c(result, lapply(res, function(x) LaTeX2range(c(j, x$path), x$range)))
      }
    }
  }
  result
}

#' @rdname find_sequence
#' @param items1,items2 Two [LaTeX2] or [LaTeX2item] objects.
#' @returns `items_are_equal` returns a logical indicator of equality after removing source references.
#' @export
items_are_equal <- function(items1, items2) {
  identical(zap_srcref(items1), zap_srcref(items2))
}
