
#' @rdname finders
#' @title Miscellaneous low-level finders
#' @name finders
#' @param items A list of latex items.
#' @param all If `FALSE`, return the first match
#' @returns `find_whitespace()` returns the indices of
#' whitespace in `items`.
#' @export
find_whitespace <- function(items, all = TRUE) {
  if (all)
    which(vapply(items, is_whitespace, TRUE))
  else {
    for (i in seq_along(items))
      if (is_whitespace(items[[i]])) return(i)
    integer()
  }
}


#' @rdname finders
#' @param envtypes Which types of environment to look for.
#' @returns `find_env()` returns the indices within `items`
#' of environments in `envtypes`.
#' @export
find_env <- function(items, envtypes, all = TRUE) {
  if (all)
    which(vapply(seq_along(items),
               function(i)
                 is_env(items[[i]]) &&
                 envName(items[[i]]) %in% envtypes,
               TRUE))
  else {
    for (i in seq_along(items))
      if (is_env(items[[i]]) &&
          envName(items[[i]]) %in% envtypes)
        return(i)
    integer()
  }
}

#' @rdname finders
#' @param macros Which types of macros to look for.
#' @returns `find_macro()` returns the index within `items`
#' of instances in `macros`.
#' @export
find_macro <- function(items, macros, all = TRUE) {
  if (all)
    which(vapply(seq_along(items),
               function(i)
                 is_macro(items[[i]]) &&
                 macroName(items[[i]]) %in% macros,
               TRUE))
  else {
    for (i in seq_along(items))
      if (is_macro(items[[i]]) &&
          macroName(items[[i]]) %in% macros)
        return(i)
    integer()
  }
}

#' @rdname finders
#' @param codes Which codes to look for.
#' @returns `find_catcode()` returns the index within `items`.
#' of specials matching `code`.
#' @export
find_catcode <- function(items, codes, all = TRUE) {
  if (all)
    which(vapply(seq_along(items),
               function(i)
                 latexTag(items[[i]]) == "SPECIAL" &&
                 catcode(items[[i]]) %in% codes,
               TRUE))
  else {
    for (i in seq_along(items))
      if (latexTag(items[[i]]) == "SPECIAL" &&
          catcode(items[[i]]) %in% codes)
        return(i)
    integer()
  }
}

#' @rdname finders
#' @param tags Which tags to look for.
#' @returns `find_tags()` returns the index within `items`.
#' of items with tags matching `tags`.
#' @export
find_tags <- function(items, tags, all = TRUE) {
  if (all)
    which(vapply(seq_along(items),
               function(i)
                 latexTag(items[[i]]) %in% tags,
               TRUE))
  else {
    for (i in seq_along(items))
      if (latexTag(items[[i]]) %in% tags)
        return(i)
    integer()
  }
}

#' @rdname finders
#' @param char Which character to look for.
#' @returns `find_char()` returns the index within `items`
#' of characters matching `char`.  Only characters
#' marked as SPECIAL by the parser will be found.
#' @export
find_char <- function(items, char, all = TRUE) {
  if (all)
    which(vapply(seq_along(items),
               function(i)
                 latexTag(items[[i]]) == "SPECIAL" &&
                 items[[i]] == char,
               TRUE))
  else {
    for (i in seq_along(items))
      if (latexTag(items[[i]]) == "SPECIAL" &&
          grepl(char, items[[i]], fixed = TRUE))
        return(i)
    integer()
  }
}

#' @rdname finders
#' @returns `find_block()` returns the index within `items`
#' of blocks (i.e. sequences in {})
#' @export
find_block <- function(items, all = TRUE) {
  if (all)
    which(vapply(seq_along(items),
               function(i)
                 latexTag(items[[i]]) == "BLOCK",
               TRUE))
  else {
    for (i in seq_along(items))
      if (latexTag(items[[i]]) == "BLOCK")
        return(i)
    integer()
  }
}

#' @title Find path to a particular kind of item
#' @param items A list of latex items.
#' @param is_fn Which test function to use.
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
#' path <- path_to(parsed, is_fn = is_env,
#'                         envtypes = "tabular")
#' @export
path_to <- function(items, is_fn, ..., all = FALSE) {
  hits <- if (all) list() else numeric()
  for (i in seq_along(items)) {
    if (is_fn(items[[i]], ...)) {
      if (!all) return(i)
      else hits <- c(hits, list(i))
    }
    if (is.list(items[[i]])) {
      recurse <- path_to(get_contents(items[[i]]),
                         all = all,
                         is_fn, ...)
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
#'
#' @returns `get_item()` returns the item at the given path.
#' @export
#'
#' @examples
#' get_item(parsed, path)
get_item <- function(items, path)
  items[[path]]

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
    item <- insert_values(item[[head]], tail, values)
    items[[head]] <- items
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
    # Try narrowing the range
    while ((n <- length(range)) > 1) {
      if (grepl(pattern, deparseLatex(items[range[-n]]), ...)) {
        range <- range[-n]
      } else
        break
    }
    while (length(range) > 1) {
      if (grepl(pattern, deparseLatex(items[range[-1]]), ...)) {
        range <- range[-1]
      } else
        break
    }
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
#' set_range(parsed, range, "The 11th item")
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
