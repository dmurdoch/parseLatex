#' @title Miscellaneous utilities
#' @name Utilities
#'
#' @param items A [LaTeX2] object or list of items, or
#' a [LaTeX2item] which is a list.
#' @param which Which items to operate on.
#' @returns `drop_items()` returns the list of items with specific items removed.
#' @export
drop_items <- function(items, which) {
  saveattr <- attributes(items)
  if (length(which))
    items <- items[-which]
  attributes(items) <- saveattr
  if (inherits(items, "LaTeX2item"))
    items
  else
    as_LaTeX2(items)
}

#' @rdname Utilities
#'
#' @returns `select_items()` returns the list of subsetted items.
#' @export
select_items <- function(items, which) {
  as_LaTeX2(items[which])
}

#' @rdname Utilities
#'
#' @returns `drop_whitespace()` returns the items with
#'  whitespace (blanks, tabs, newlines) removed.

#' @note `drop_whitespace()` will drop the whitespace that separates text items, so deparsing will merge
#' them into a single item.
#'
#' @seealso `drop_whitespace()` does not act recursively; use [reduce_whitespace] for that.
#' @export
drop_whitespace <- function(items)
  drop_items(items, find_whitespace(items))

#' @rdname Utilities
#'
#' @returns `trim_whitespace()` returns the items with
#'  leading and trailing whitespace (blanks, tabs, newlines) removed.
#' @export
trim_whitespace <- function(items) {
  saveattr <- attributes(items)
  while(length(items) && is_whitespace(items[[length(items)]]))
    items <- items[-length(items)]
  while(length(items) && is_whitespace(items[[1]]))
    items <- items[-1]
  attributes(items) <- saveattr
  if (inherits(items, "LaTeX2item"))
    items
  else
    as_LaTeX2(items)
}

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

#' Splitting lists of items
#' @name splitting
#' @param splits Which item numbers divide the parts?
#' @param include If `TRUE`, include the split item at
#' the end of each part.
#' @returns `split_list()` returns a list of pieces
#' separated at the splits.
#' @export
split_list <- function(items, splits, include = FALSE) {
  prev <- 0
  result <- lapply(splits, function(s) {
    if (include)
      sel <- items[(prev + 1):s]
    else if (s > prev + 1)
      sel <- items[(prev + 1):(s - 1)]
    else
      sel <- items[FALSE]
    prev <<- s
    sel
  })
  # Add in one more after the last split
  extras <- items[seq_along(items) > max(splits, 0)]
  result <- c(result, list(extras))
  result
}

#' @rdname splitting
#' @param ... Arguments to pass to `split_list`.
#' @returns `split_latex()` returns a list of pieces
#' separated at the splits.  Each piece is marked as
#' an `ITEMLIST` item, and the whole thing is
#' also marked that way.
#' @export
split_latex <- function(...) {
  items <- split_list(...)
  items <- lapply(items, new_itemlist)
  new_itemlist(items)
}


#' @rdname Utilities
#' @param item A non-list [LaTeX2item].
#' @param split Where to split the characters.
#' @returns `split_chars()` returns a [LaTeX2]
#' list containing the result of calling [strsplit]
#' on the text of the `item`.
#' @examples
#' parsed <- parseLatex("Hello")
#' unclass(parsed)
#' unclass(split_chars(parsed[[1]]))
#' @export
split_chars <- function(item, split = "") {
  stopifnot(is.character(item))
  if (nchar(item) <= 1)
    return(latex2(item))
  chars <- strsplit(item, split)
  result <- lapply(chars[[1]], function(x) {
    attributes(x) <- attributes(item)
    x}
  )
  latex2(result)
}

#' Convenience functions to get or set contents of item
#'
#' @param item An item from a Latex list (or a [LaTeX2] list with one item).
#'
#' @returns `get_contents` returns the contents of the item as a [LaTeX2] list.
#' @export
#' @examples
#' get_contents(parseLatex("{abc}"))
#'
get_contents <- function(item) {

  if (inherits(item, "LaTeX2")) {
    if (length(item) > 1)
      warning("only using the first element")
    item <- item[[1]]
  }
  if (is.list(item)) {
    attributes(item) <- NULL
    as_LaTeX2(item)
  } else
    NULL
}

#' @rdname get_contents
#' @param value An object that can be coerced to be
#' a [LaTeX2] object.
#'
#' @returns `set_contents` returns the original `item` with the contents
#' replaced by `value`.
#' @export
#'
#' @examples
#' set_contents(parseLatex("{abc}"), "def")
set_contents <- function(item, value) {
  if (!inherits(item, "LaTeX2item") && is.list(item)) {
    if (length(item) > 1)
      warning("only using the first element")
    item <- item[[1]]
  }
  stopifnot(inherits(item, "LaTeX2item") && is.list(item))
  value <- as_LaTeX2(value)
  range <- LaTeX2range(NULL, NULL)
  set_range(item, range, value)
}

#' @rdname Utilities
#' @param ... Items to be passed to `latex2()`.
#' @returns `new_block()` returns a `BLOCK` item containing the items.
#' @export
#'
#' @examples
#' new_block(parseLatex("abc"))
new_block <- function(...) {
  items <- latex2(...)
  attr(items, "latex_tag") <- "BLOCK"
  class(items) <- "LaTeX2item"
  items
}

#' @rdname Utilities
#' @param name The desired environment name.
#' @returns `new_env()` returns an environment item containing the other items.
#' @export
#'
#' @examples
#' new_env("itemize", parseLatex("\\item An item"))
new_env <- function(name, ...) {
  result <- latex2(...)
  if (length(result) == 0 ||
      !is_char(result[[1]], "\n"))
    insert_values(result, 1, "\n")
  n <- length(result)
  if (!is_char(result[[n]], "\n"))
    insert_values(result, n+1, "\n")
  attr(result, "latex_tag") <- "ENVIRONMENT"
  class(result) <- "LaTeX2item"
  envName(result) <- name
  result
}

#' @name itemlist
#' @aliases ITEMLIST
#' @title Lists of items
#' @param ... Items to be passed to `latex2()`.
#' @details An `ITEMLIST` is a list of lists of items.
#' Deparsing it just concatenates the parts.  This is
#' intended to be used when parsing tables, for example,
#' where it makes sense to break up the table into
#' individual rows.  See `split_table` for more details.
#' @returns `new_itemlist()` returns an `ITEMLIST` item containing the items.
#' @export
#'
#' @examples
#' new_itemlist(parseLatex("abc def"), label = "items")
new_itemlist <- function(...) {
  items <- latex2(...)
  attr(items, "latex_tag") <- "ITEMLIST"
  class(items) <- "LaTeX2item"
  items
}

#' @rdname itemlist
#' @param items A list of [LaTeX2item] objects.
#' @param recursive Whether to proceed recursively.
#' @returns `flatten_itemlists()` returns `items` with
#'  `ITEMLIST` items expanded.  If `items` itself was an
#'  `ITEMLIST`, it is returned as a [LaTeX2] object;
#'  otherwise its type will be unchanged.
#'  The result will never
#'  include any `ITEMLIST` or `PLACEHOLDER` items at the top level,
#'  and if `recursive` is `TRUE`, not at any level.
#' @export
flatten_itemlists <- function(items, recursive = FALSE) {
  attrs <- attributes(items)
  i <- 1
  while (i <= length(items)) {
    if (is_itemlist(items[[i]])) {
      items <- insert_values(items, i + 1, items[[i]])
      items <- drop_items(items, i)
    } else if (is_placeholder(items[[i]])) {
      items <- drop_items(items, i)
    } else {
      if (recursive && is.list(items[[i]]))
        items[[i]] <- flatten_itemlists(items[[i]], recursive = TRUE)
      i <- i + 1
    }
  }
  tag <- attrs$latex_tag
  if (!is.null(tag) && tag == "ITEMLIST")
    items <- as_LaTeX2(items)

  items
}

#' @rdname itemlist
#' @aliases PLACEHOLDER
#' @returns `placeholder()` returns a `LaTeX2item`
#' object with tag `PLACEHOLDER`.  These will never
#' print, and are used as spacers within an `ITEMLIST`.
#' @export
placeholder <- function() {
  structure(character(), class = "LaTeX2item",
            latex_tag = "PLACEHOLDER")
}
