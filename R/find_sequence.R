
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
#' @returns `find_sequence()` returns a [LaTeX2range] or list of them where `sequence` occurs within `items`.
#' @details `find_sequence()` will only match sequences that
#' are entirely contained within a single list in `items`.
#' Thus if [prepare_table()] is called on a table, then
#' `find_sequence()` will find sequences in the code before
#' or after the rows, or entirely within a single cell,
#' but not crossing alignment markers (`"&"`).
#' @export
#' @examples
#' find_sequence(parseLatex("a & b & c"), "b & c")
find_sequence <- function(items, sequence, all = FALSE,
                          ignore_whitespace = TRUE) {
  sequence <- as_LaTeX2(sequence)
  if (ignore_whitespace)
    sequence <- zap_whitespace(sequence)
  attributes(sequence) <- NULL

  result <- NULL
  n <- length(sequence)
  lendiff <- length(items) - n
  if (lendiff >= 0L) {
    low <- NA
    high <- NA
    i <- 0L  # the character in the sequence
    while (i < length(items)) {
      i <- i + 1L
      good <- TRUE
      for (j in 1:n) {
        if (ignore_whitespace)
          while(i <= length(items) && is_whitespace(items[[i]]))
            i <- i + 1L
        if (j == 1) low <- i
        if (i > length(items) || !items_are_equal(items[[i]], sequence[[j]])) {
          good <- FALSE
          break
        }
        if (j == n) high <- i
        i <- i + 1
      }
      if (good) {
        res <- LaTeX2range(NULL, low:high)
        if (!all)
          return(res)
        else
          result <- c(result, list(res))
      }
      i <- low
    }
  }

  for (i in seq_along(items)) {
    if (is.list(items[[i]])) {
      res <- find_sequence(items[[i]], sequence, all = all,
                           ignore_whitespace = ignore_whitespace)
      if (length(res)) {
        if (!all)
          return(LaTeX2range(c(i, res$path), res$range))
        else
          result <- c(result, lapply(res, function(x) LaTeX2range(c(i, x$path), x$range)))
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
  identical(rmSrcrefs(items1), rmSrcrefs(items2))
}
