
#' @rdname finders
#' @title Miscellaneous low-level finders
#' @name finders
#' @param items A list of latex items.
#' @returns `find_whitespace()` returns the indices of
#' whitespace in `items`.
#' @export
find_whitespace <- function(items, ...)
  find_general(items, is_whitespace, ...)


#' @rdname finders
#' @param envtypes Which types of environment to look for.
#' @returns `find_env()` returns the indices within `items`
#' of environments in `envtypes`.
#' @export
find_env <- function(items, envtypes = NULL, ...)
  find_general(items, is_env, envtypes = envtypes, ...)

#' @rdname finders
#' @param macros Which types of macros to look for.
#' @returns `find_macro()` returns the index within `items`
#' of instances in `macros`.
#' @export
find_macro <- function(items, macros = NULL, ...)
  find_general(items, is_macro, macros = macros, ...)

#' @rdname finders
#' @param codes Which codes to look for.
#' @returns `find_catcode()` returns the index within `items`.
#' of specials matching `code`.
#' @export
find_catcode <- function(items, codes, ...)
  find_general(items, function(x)
    latexTag(x) == "SPECIAL" && catcode(x) %in% codes,
    ...)

#' @rdname finders
#' @param tags Which tags to look for.
#' @returns `find_tags()` returns the index within `items`.
#' of items with tags matching `tags`.
#' @export
find_tags <- function(items, tags, ...)
  find_general(items, function(x) latexTag(x) %in% tags,
               ...)

#' @rdname finders
#' @param char Which character to look for.
#' @returns `find_char()` returns the index within `items`
#' of characters matching `char`.  Only characters
#' marked as SPECIAL by the parser will be found.
#' @export
find_char <- function(items, char, ...)
  find_general(items, function(x) latexTag(x) == "SPECIAL" && x == char, ...)


#' @rdname finders
#' @returns `find_block()` returns the index within `items`
#' of blocks (i.e. sequences in {})
#' @export
find_block <- function(items, ...)
  find_general(items, is_block, ...)


#' @title Find path to a particular kind of item
#' @param items A list of latex items.
#' @param test Which test function to use.
#' @param ... Additional parameters to pass to `is_fn`.
#' @param all Return all paths, or just the first?
#' @details `path_to()` does a recursive search in the order
#' items appear in the deparse.
#' @returns `path_to()` returns the recursive path to the
#' first example matching the `is_fn` conditions,
#' or a list of paths to all matching items.
#' @examples
#' latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex", caption = "Sample table")
#' parsed <- parseLatex(latex)
#' parsed
#' path <- path_to(parsed, test = is_env,
#'                         envtypes = "tabular")
#' @export
path_to <- function(items, test, ..., all = FALSE) {
  hits <- if (all) list() else numeric()
  for (i in seq_along(items)) {
    if (test(items[[i]], ...)) {
      if (!all) return(i)
      else hits <- c(hits, list(i))
    }
    if (is.list(items[[i]])) {
      recurse <- path_to(get_contents(items[[i]]),
                         all = all,
                         test, ...)
      if (all)
        hits <- c(hits, lapply(recurse, function(x) c(i, x)))
      else if (length(recurse))
        return(c(i, recurse))
    }
  }
  hits
}

#' @rdname path_to
#'
#' @param path Integer vector of subitems
#' @param index Index into the flattened version of `items`.
#'
#' @returns `get_item()` returns the item at the given path.  If `index` is specified, `get_item()` will return that
#' item in the flattened version of `items`.
#' @export
#'
#' @examples
#' get_item(parsed, path)
get_item <- function(items, path = index_to_path(index, items),
                     index)
  items[[path]]

#' @rdname path_to
#'
#' @param paths List of paths
#' @param indices Vector of indices into the flattened version of `items`.
#'
#' @returns `get_items()` returns the items at the given paths as a [LaTeX2] object.  If `index` is specified, `get_items()` will return those
#' items in the flattened version of `items`.
#' @export
get_items <- function(items, paths = lapply(indices, index_to_path, items), indices) {
  latex2(lapply(paths, get_item, items = items))
}

#' @rdname path_to
#' @param value A [LaTeX2item] to set as a value.
#' @returns `set_item()` replaces the item at the given path, and returns the modified version of `items`.
#' @export
set_item <- function(items, path, value) {
  if (!inherits(value, "LaTeX2item"))
    stop("requires a LaTeX2item as a value")
  items[[path]] <- value
  items
}

#' @rdname path_to
#' @param values A [LaTeX2] list or a [LaTeX2item].
#' @returns `insert_values()` inserts the `values` before the item mentioned in `path`, and returns the modified version of `items`.
#' @export
insert_values <- function(items, path, values) {
  if (length(path) > 1) {
    head <- path[-length(path)]
    tail <- path[length(path)]
    item <- insert_values(items[[head]], tail, values)
    items[[head]] <- item
  } else {
    # length 1 path
    values <- latex2(values)
    n <- length(values)
    if (path <= length(items))
      items[(path:length(items)) + n] <- items[path:length(items)]
    items[seq_along(values) - 1 + path] <- values[]
  }
  items
}

#' @rdname path_to
#' @returns `get_container()` returns the item
#' containing the given path
#' @export
get_container <- function(items, path) {
  if (length(path)) {
    head <- path[-length(path)]
    if (!length(head))
      items
    else
      items[[head]]
  } else
    items
}

#' @rdname path_to
#'
#' @returns `get_which()` returns the index
#' of the item within its container.
#' @export
get_which <- function(path)
  path[length(path)]

#' @title Find a pattern in deparsed items
#' @param items A list of latex items.
#' @param all Find all matching, or the first?
#' @param pattern Pattern to use in `grepl()`.
#' @param ... Additional parameters to pass to `grepl`.
#' @description Searches a [LaTeX2] list for text using `grepl()` on deparsed versions of parts of the code.
#' It attempts to find the narrowest match(es) that lie
#' within a single container.
#' @details `find_pattern()` does a recursive search in the order
#' items appear in the deparse.  If the pattern matches,
#' it attempts to narrow the match by recursing into
#' containers and dropping earlier and later items.
#' It should always return syntactically correct LaTeX
#' code in which the pattern appears.
#' @returns `find_pattern()` returns a [LaTeX2range] object
#' or (if `all` is `TRUE`) a list of them.
#' @examples
#' latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex", caption = "Sample table")
#' parsed <- parseLatex(latex)
#' parsed
#' loc <- find_pattern(parsed, "RX4 Wag", fixed = TRUE)
#' loc
#' print(loc, source = parsed)
#' @export
find_pattern <- function(items, pattern, ..., all = FALSE) {
  hits <- list()
  items0 <- items
  deletes <- 0
  range <- seq_along(items)
  while (grepl(pattern, deparseLatex(items[range]), ...)) {
    # Try narrowing the range using bisection
    low <- 1
    high <- length(range)
    while (low < high) {
      mid <- floor((low + high) / 2)
      if (grepl(pattern, deparseLatex(items[range[1:mid]]), ...))
        high <- mid
      else
        low <- mid + 1
    }
    high <- n <- low
    low <- 1
    while (low < high) {
      mid <- ceiling((low + high) / 2)
      if (grepl(pattern, deparseLatex(items[range[mid:n]]), ...))
        low <- mid
      else
        high <- mid - 1
    }
    range <- range[low:n]
    if (length(range) == 1 &&
        is.list(items[[range]])) {
      recurse <- find_pattern(get_contents(items[[range]]),
                              pattern = pattern,
                              all = all, ...)
      if (all)
        hits <- c(hits, lapply(recurse, function(x)
          LaTeX2range(path = c(range, x$path),
                      range = x$range)))
      else return(LaTeX2range(path = c(range, recurse$path),
                       range = recurse$range))
    } else {
      result <- LaTeX2range(path = numeric(),
                            range = range)
      if (all)
        hits <- c(hits, list(result))
      else
        return(result)
    }
    # If we got here, all must be TRUE, and we
    # need to keep looking
    range <- max(range) + 1
    if (range > length(items))
      break
    range <- range:length(items)
  }
  hits
}

#' Ranges within LaTeX2 lists.
#' @param path An integer vector to use as a path.
#' @param range A range of values within the path.
#' @details
#' LaTeX2range objects are lists with `path` and `range` entries.  `path` is a recursive index
#' into a [LaTeX2] list, and `range` is a range of
#' entries in the result.
#'
#' If `path` is `NULL`, the object refers to the entire
#' source object.  If `range` is `NULL`, it refers
#' to the whole [LaTeX2item] given by the `path`.
#'
#' @returns `LaTeX2range()` returns a constructed `LaTeX2range` object.
#' @export
LaTeX2range <- function(path, range)
  structure(list(path = path, range = range),
            class = "LaTeX2range")

#' Convert between paths and indices
#' @param path A vector of integers, assumed to be
#' a path through "ITEMLIST" entries in a [LaTeX2]
#' or [LaTeX2item] object.
#' @param items The referenced object.
#' @returns `path_to_index` returns a scalar value
#' corresponding the the index if `items` was flattened.
#' @seealso [flatten_itemlists()]
#' @export
#' @examples
#' latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
#' parsed <- parseLatex(latex)
#' tablepath <- path_to(parsed, is_env, envtypes = "tabular")
#' table <- prepare_table(parsed[[tablepath]])
#' path_to_index(c(4,1,1), table)
path_to_index <- function(path, items) {
  n <- length(path)
  if (n == 0)
    stop("path has no entries")

  result <- 0L
  current <- items
  for (i in seq_along(path)) {
    for (j in seq_len(path[i] - 1)) {
      if (is_itemlist(current[[j]]))
        result <- result + attr(current[[j]], "truecount")
      else
        result <- result + 1L
    }
    current <- current[[path[i]]]
    if (is_itemlist(current) == (i == n))
      stop("path must only traverse itemlist items and end at something else.")
  }
  result + 1L
}

#' @rdname path_to_index
#' @param index A scalar integer which would be
#' the index to an item if `items` was flattened.
#' @returns `index_to_path` returns a vector of integers
#' which would index the specified item.
#' @export
#' @examples
#' index_to_path(3, table)
#'
index_to_path <- function(index, items) {
  path <- integer()
  current <- 0
  i <- 0
  while (current < index) {
    i <- i + 1
    if (is_itemlist(items[[i]])) {
      truecount <- attr(items[[i]], "truecount")
      if (current + truecount >= index)
        return(c(i, index_to_path(index - current, items[[i]])))
      else
        current <- current + truecount
    } else
      current <- current + 1
  }
  i
}

#' @rdname path_to_index
#' @param path1,path2 Paths into the same destination list.
#' @returns `paths_to_range` returns a list of [LaTeX2range] objects covering all entries
#' extending from `path1` to `path2`.
#' @export
#' @examples
#' ranges <- paths_to_ranges(index_to_path(3, table),
#'                           c(4,1,1), table)
#' lapply(ranges, get_range, items = table)
#'
paths_to_ranges <- function(path1, path2, items) {
  common <- path1
  for (i in seq_along(path1)) {
    if (length(path2) < i)
      stop("path2 overlaps path1")
    if (path2[i] < path1[i])
      stop("path2 precedes path1")
    if (path2[i] > path1[i]) {
      common <- path1[-(i:length(path1))]
      if (length(common)) {
        items <- items[[common]]
        path1 <- path1[-seq_along(common)]
        path2 <- path2[-seq_along(common)]
      }
      break
    }
  }
  if (length(common) == length(path1)) {
    # no differences seen
    if (length(common) < length(path2))
      stop("path1 overlaps path2")
    return(list(LaTeX2range(path1, NULL)))
  }
  # Now we have path1 different from path2 from the start
  # Form a list of ranges
  result <- list()
  n <- length(path1)
  for (i in rev(seq_along(path1)[-1L])) {
    path <- path1[-(i:n)]
    if (length(path))
      len <- length(items[[path]])
    else
      len <- length(items)
    if (i < n && path1[i] < len)
      result <- c(result, list(LaTeX2range(c(common, path1[-(i:n)]),
                                         (path1[i]+1L):len)))
    else # the last one
      result <- c(result, list(LaTeX2range(c(common, path1[-(i:n)]),
                                           path1[i]:len)))

  }
  if (path1[1L] < path2[1L] - 1L)
    for (i in (path1[1L]+1L):(path2[1L]-1L))
      result <- c(result, list(LaTeX2range(c(common, i), NULL)))
  n <- length(path2)
  for (i in seq_along(path2)[-1L]) {
    if (i < n) {
      if (path2[i] > 1L)
        result <- c(result, list(LaTeX2range(c(common, path2[-(i:n)]),
                                         1L:(path2[i] - 1L))))
    } else
      result <- c(result, list(LaTeX2range(c(common, path2[-(i:n)]),
                                           1L:path2[i])))
  }
  result
}

#' @rdname LaTeX2range
#' @param x Object to print.
#'
#' @param source Optional parsed list from which to
#' extract the range.
#' @param ... Ignored.
#'
#' @export
print.LaTeX2range <- function(x, source = NULL, ...) {
  if (!is.null(source))
    print(as_LaTeX2(source[[x$path]][x$range]))
  else {
    cat("path=")
    cat(x$path, sep = ",")
    if (length(x$range) > 0)
      cat(" range=", min(x$range), ":", max(x$range), "\n", sep = "")
    else
      cat(" range=all\n")
  }
  invisible(x)
}

#' Set items in a [LaTeX2] object
#' @param items A [LaTeX2] object or other list of
#' [LaTeX2item] objects.
#' @param range A [LaTeX2range] object.
#' @param values An object that can be coerced to
#' a [LaTeX2] object or (if `range$range` is `NULL`)
#' a [LaTeX2item].
#' @returns `set_range()` replaces the item(s) at the given path, and returns the modified version of `items`.
#' @examples
#' latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex", caption = "Sample table")
#' parsed <- parseLatex(latex)
#' tablepath <- path_to(parsed, is_env, envtypes = "tabular")
#' range <- LaTeX2range(tablepath, 11)
#' parsed <- set_range(parsed, range, "The 11th item")
#' parsed
#' @export
set_range <- function(items, range, values) {
  path <- range$path
  range <- range$range
  if (!is.null(path) && !is.null(range)) {
    range <- LaTeX2range(NULL, range)
    items[[path]] <- set_range(items[[path]], range, values)
  } else if (!is.null(path)) { # but NULL range
    newpath <- path[-length(path)]
    if (!length(newpath)) newpath <- NULL
    range <- LaTeX2range(newpath, path[length(path)])
    items <- set_range(items, range, values)
  } else {
    setlen <- function(items, len) {
      saveattr <- attributes(items)
      length(items) <- len
      attributes(items) <- saveattr
      items
    }
    values <- as_LaTeX2(values)
    n <- length(values)
    if (is.null(range)) {
      saveattr <- attributes(items)
      items <- setlen(items, length(values))
      items[seq_along(values)] <- values
    } else {
      iold <- seq_along(items)
      if (length(iold)) {
        shift <- length(values) - (max(range) - min(range) + 1)
        after <- iold[iold > max(range)]
        if (length(after) && shift != 0)
          items[after + shift] <- items[after]
        if (shift < 0)
          items <- setlen(items, length(items) + shift)
      }
      items[min(range) - 1 + seq_len(n)] <- values
    }
  }
  items
}

#' @rdname set_range
#' @returns `get_range()` extracts the
#' specified range and returns it as a [LaTeX2] object.
#' @examples
#' get_range(parsed, range)
#' @export

get_range <- function(items, range) {
  if (length(range$path))
    items <- items[[range$path]]
  if (length(range$range)) {
    idx <- seq_along(items)
    items <- items[idx >= min(range$range) &
                   idx <= max(range$range)]
  }
  as_LaTeX2(items)
}

#' @rdname path_to_index
#' @param ranges A list of [LaTeX2range] objects, such as
#' that produced by [paths_to_ranges()].
#' @returns `get_ranges()` extracts the
#' specified ranges, concatenates them, and returns them as a [LaTeX2] object.
#' @examples
#' get_ranges(table, ranges)
#' @export
get_ranges <- function(items, ranges) {
  lapply(ranges, get_range, items = items) |>
    lapply(new_itemlist) |>
    latex2() |>
    flatten_itemlists()
}

#' @rdname finders
#' @param test Test function for target.
#' @param ... For `find_general`, additional arguments
#' to pass to `test`.  For the other `find_*` functions,
#' additional arguments to pass to `find_general`.
#' @param all If `FALSE`, return just the first match
#' @param path If `TRUE`, return a path rather than an index.  See Details below.
#' @details
#' These functions search through `items` for individual
#' objects that match a test.  In general they do not
#' operate recursively, with one exception.  If `items`
#' contains `ITEMLIST` objects, the search will always
#' recurse into those.
#'
#' By default the return value is an index or a vector
#' of indices of the matches.  These are the indices as
#' they would be if any `ITEMLIST` objects had been
#' flattened.
#'
#' However, if `path = TRUE`, the path to the
#' object will be returned.  With `all = FALSE`, this will
#' be a numeric vector such that `items[[result]]` is the
#' matching item.  With `all = TRUE` it will be a list of
#' such vectors.
#' @seealso [index_to_path()], [path_to_index()]
#' @returns `find_general()` returns locations of
#'  objects matching the `test`.
#' @export

find_general <- function(items, test, ..., all = TRUE,
                         path = FALSE) {
  if (all) {
    result <- list()
    for (i in seq_along(items)) {
      if (is_itemlist(items[[i]]))
        result <- c(result,
                    lapply(find_general(items[[i]], test, ..., all = TRUE, path = TRUE),
                           function(x) c(i, x)))
      if (test(items[[i]], ...))
        result <- c(result, list(i))
    }
    if (path)
      result
    else
      vapply(result, path_to_index, 0L, items)
  } else {
    for (i in seq_along(items))
      if (test(items[[i]], ...))
        return(i)
      else if (is_itemlist(items[[i]])) {
        result <- find_general(items[[i]], test, ..., all = FALSE, path = TRUE)
        if (!is.null(result)) {
          result <- c(i, result)
          if (path)
            return(result)
          else
            return(path_to_index(result, items))
        }
      }
    NULL
  }
}
