
#' @rdname finders
#' @title Miscellaneous low-level finders
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
#' parsed[[path]]
#' @export
path_to <- function(items, all = FALSE, is_fn, ...) {
  hits <- if (all) list() else numeric()
  for (i in seq_along(items)) {
    if (is_fn(items[[i]], ...)) {
      if (!all) return(i)
      else hits <- c(hits, list(i))
    }
    idx <- if (is_env(items[[i]])) 2
           else if (is_block(items[[i]])) numeric()
           else NULL
    if (!is.null(idx)) {
      recurse <- path_to(get_contents(items[[i]]),
                         all = all,
                         is_fn, ...)
      if (all)
        hits <- c(hits, lapply(recurse, function(x) c(i, idx, x)))
      else if (length(recurse))
        return(c(i, idx, recurse))
    }
  }
  hits
}


#' @title Find a pattern in deparsed items
#' @param items A list of latex items.
#' @param all Find all matching, or the first?
#' @param pattern Pattern to use in `grepl()`.
#' @param ... Additional parameters to pass to `grepl`.
#' @description Searches a `LaTeX2` list for text using `grepl()` on deparsed versions of parts of the code.
#' It attempts to find the narrowest match(es) that lie
#' within a single container.
#' @details `find_pattern()` does a recursive search in the order
#' items appear in the deparse.  If the pattern matches,
#' it attempts to narrow the match by recursing into
#' containers and dropping earlier and later items.
#' It should always return syntactically correct LaTeX
#' code in which the pattern appears.
#' @returns `find_pattern()` returns a `LaTeX2range` item
#' or (if `all` is `TRUE`) a list of them.  `LaTeX2range`
#'  items are lists containing
#' `path` and `range`, where `path` is the recursive path
#' to the container holding the `range` of items
#' matching the `pattern`, or a list of such lists.
#' @examples
#' latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex", caption = "Sample table")
#' parsed <- parseLatex(latex)
#' parsed
#' loc <- find_pattern(parsed, "RX4 Wag", fixed = TRUE)
#' loc
#' print(loc, source = parsed)
#' @export
find_pattern <- function(items, pattern, all = FALSE, ...) {
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
      idx <- if (tag == "BLOCK") numeric() else 2
      if (all)
        hits <- c(hits, lapply(recurse, function(x)
          LaTeX2range(path = c(range, idx, x$path),
                      range = deletes + x$range)))
      else return(LaTeX2range(path = c(range, idx, recurse$path),
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

#' @rdname find_pattern
#'
#' @param path An integer vector to use as a path.
#' @param range A range of values within the path.
#'
#' @returns `LaTeX2range` returns a constructed `"LaTeX2range` object.
#' @export
LaTeX2range <- function(path, range)
  structure(list(path = path, range = range),
            class = "LaTeX2range")

#' @rdname find_pattern
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
    cat(" range=", min(x$range), ":", max(x$range), "\n", sep = "")
  }
  invisible(x)
}
