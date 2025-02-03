
#' @rdname finders
#' @title Miscellaneous low-level finders
#' @name finders
#' @param items A list of latex items.
#' @returns `find_whitespace()` returns the indices of
#' whitespace in `items`.
#' @export
find_whitespace <- function(items) {
  seq_along(items)[sapply(items, is_whitespace)]
}


#' @rdname finders
#' @param envtypes Which types of environment to look for.
#' @returns `find_env()` returns the indices within `items`
#' of environments in `envtypes`.
#' @export
find_env <- function(items, envtypes) {
  which(sapply(seq_along(items),
               function(i)
                 is_env(items[[i]]) &&
                 envName(items[[i]]) %in% envtypes))
}

#' @rdname finders
#' @param macros Which types of macros to look for.
#' @returns `find_macro()` returns the index within `items`
#' of instances in `macros`.
#' @export
find_macro <- function(items, macros) {
  which(sapply(seq_along(items),
               function(i)
                 is_macro(items[[i]]) &&
                 macroName(items[[i]]) %in% macros))
}

#' @rdname finders
#' @param codes Which codes to look for.
#' @returns `find_catcode()` returns the index within `items`.
#' of specials matching `code`.
#' @export
find_catcode <- function(items, codes) {
  which(sapply(seq_along(items),
               function(i)
                 latexTag(items[[i]]) == "SPECIAL" &&
                 catcode(items[[i]]) %in% codes))
}

#' @rdname finders
#' @param tags Which tags to look for.
#' @returns `find_tags()` returns the index within `items`.
#' of items with tags matching `tags`.
#' @export
find_tags <- function(items, tags) {
  which(sapply(seq_along(items),
               function(i)
                 latexTag(items[[i]]) %in% tags))
}

#' @rdname finders
#' @param char Which character to look for.
#' @returns `find_char()` returns the index within `items`
#' of characters matching `char`.  Only characters
#' marked as SPECIAL by the parser will be found.
#' @export
find_char <- function(items, char) {
  which(sapply(seq_along(items),
               function(i)
                 latexTag(items[[i]]) == "SPECIAL" &&
                 items[[i]] == char))
}

#' @title Find path to a particular kind of item
#' @param items A list of latex items.
#' @param all Find all matching, or the first?
#' @param is_fn Which test function to use.
#' @param ... Additional parameters to pass to `is_fn`.
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
    if (is_env(items[[i]]) || is_block(items[[i]])) {
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
  while (grepl(pattern, deparseLatex(items), ...)) {
    # Try narrowing the range
    range <- seq_along(items)
    while ((n <- length(items)) > 1) {
      if (grepl(pattern, deparseLatex(items[-n]), ...)) {
        items <- items[-n]
        range <- range[-n]
      } else
        break
    }
    while (length(items) > 1) {
      if (grepl(pattern, deparseLatex(items[-1]), ...)) {
        items <- items[-1]
        range <- range[-1]
      } else
        break
    }
    if (length(range) == 1 &&
        (tag <- latexTag(items0[[deletes + range]])) %in%
        c("BLOCK", "ENVIRONMENT")) {
      recurse <- find_pattern(get_contents(items0[[deletes + range]]),
                              pattern = pattern,
                              all = all, ...)
      if (all)
        hits <- c(hits, lapply(recurse, function(x)
          LaTeX2range(path = c(range, x$path),
                      range = deletes + x$range)))
      else return(LaTeX2range(path = c(range, recurse$path),
                       range = deletes + recurse$range))
    } else {
      result <- LaTeX2range(path = numeric(),
                            range = deletes + range)
      if (all)
        hits <- c(hits, list(result))
      else
        return(result)
    }
    # If we got here, all must be TRUE, and we
    # need to keep looking
    deletes <- deletes + max(range)
    keep <- seq_along(items0) > deletes
    items <- items0[keep]
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
    if (!is.null(x$range))
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
#' range <- LaTeX2range(tablepath, 8)
#' set_range(parsed, range, "The 8th item")
#' @export
set_range <- function(items, range, values) {
  path <- range$path
  range <- range$range
  values <- as_LaTeX2(values)
  if (!length(path))
    item <- items
  else
    item <- items[[path]]
  if (!length(range))
    range <- seq_along(item)
  iold <- seq_along(item)
  saveattr <- attributes(item)
  item <- c(item[iold < min(range)],
            values,
            item[iold > max(range)])
  attributes(item) <- saveattr
  if (!length(path))
    items <- item
  else
    items[[path]] <- item

  items
}
