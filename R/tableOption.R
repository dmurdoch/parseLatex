#' @rdname tableOption
#' @name tableOption
#' @title Functions related to table options.
#' @param table A known tabular-like environment object,
#' or the contents of one.
#' @returns `find_posOption()` returns the indices of the
#' entries corresponding to the "pos" option, including the
#' brackets, within the table.
#' @examples
#' latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
#' parsed <- parseLatex(latex)
#' table <- parsed[[find_tabular(parsed)]]
#' table
#' find_posOption(table)
#'
#' @export
find_posOption <- function(table) {
  has_itemlist <- has_itemlist(table)
  if (has_itemlist) {
    content <- table[[1]]
  } else
    content <- table
  if (is_env(table, "tabu"))
    start <- min(c(find_char(content, "[", all = FALSE),
                   find_block(content, all = FALSE)))
  else
    start <- 1
  result <- find_bracket_options(content, start = start)
  if (has_itemlist)
    result$path <- c(1L, result$path)
  result
}

#' @rdname tableOption
#' @returns `posOption()` returns a [LaTeX2] object containing the
#' "pos" option.
#' @examples
#' posOption(table)
#'
#' @export
posOption <- function(table) {
  as_LaTeX2(get_range(table, find_posOption(table)))
}

#' @param value A character string or [LaTeX2] object.
#' @param asis Whether to make small modifications in replacement functions.
#' @details Unless `asis == TRUE`, the value for `value` in `posOption(table) <- value`
#' can be specified with or without the enclosing brackets.
#' @rdname tableOption

#' @examples
#' posOption(table) <- "h"
#' posOption(table)
#' @export
`posOption<-` <- function(table, asis = FALSE, value) {
  bracket_options(table, asis = asis) <- value
  table
}

#' @rdname tableOption
#' @returns `find_widthOption()` returns the index of the
#' block corresponding to the "width" option, if there is one.
#' Only some tabular-like environments have these.
#' @examples
#' find_widthOption(table)
#'
#' @export
find_widthOption <- function(table) {
  if(!is_env(table, envtypes = c("tabular*", "tabularx", "tabulary")))
    NULL
  else
    find_brace_options(table)
}

#' @rdname tableOption
#' @returns `widthOption()` returns a [LaTeX2] object containing the
#' "width" option, if the table has one.
#' @examples
#' widthOption(table)
#'
#' @export
widthOption <- function(table) {
  if(!is_env(table, envtypes = c("tabular*", "tabularx", "tabulary")))
    NULL
  else
    as_LaTeX2(table[find_widthOption(table)])
}

#' @rdname tableOption
#' @export
`widthOption<-` <- function(table, asis = FALSE, value) {
  if (is_env(table)) {
    if(envName(table) %in% c("tabular", "longtable")) {
      warning("tables of type ", dQuote(envName(table)),
              " do not support a width option.  No change made.")
      return(table)
    }
  }
  brace_options(table, asis = asis) <- value
  table
}

#' @rdname tableOption
#' @returns `find_columnOptions()` returns a [LaTeX2range]
#' object for the column options of the table.
#' @examples
#' find_columnOptions(table)
#' @export
find_columnOptions <- function(table) {
  table0 <- table
  has_itemlist <- has_itemlist(table)
  if (has_itemlist)
    table <- table[[1]]

  start <- 1L
  which <- 1L
  if (envName(table0) %in% c("tabular*", "tabularx", "tabulary"))
    which <- 2L

  if (envName(table0) == "tabu") {
    while (!is_bracket(table[[start]], "[") &&
           !is_block(table[[start]])) start <- start + 1L
  }
  result <- find_brace_options(table, which = which, start = start)
  if (has_itemlist)
    LaTeX2range(1L, result)
  else
    LaTeX2range(NULL, result)
}

#' @rdname tableOption
#' @returns `columnOptions()` returns a [LaTeX2] object containing the
#' "column" options.
#' @examples
#' columnOptions(table)
#'
#' @export
columnOptions <- function(table) {
  get_range(table, find_columnOptions(table))
}

#' @rdname tableOption
#' @param column For which column?
#' @returns `columnOption()` returns a [LaTeX2] object
#' containing the requested column option.  A `"|"` divider
#' will not be included.
#' @export
#' @examples
#' columnOption(table, 3)
columnOption <- function(table, column) {
  opts <- get_contents(columnOptions(table))
  result <- NULL
  start <- 0
  letters <- find_tags(opts, "TEXT", all = TRUE)
  for (i in seq_along(letters)) {
    x <- opts[[letters[i]]]
    stop <- start + nchar(x)
    if (column <= stop) {
      j <- column - start
      result <- latex2(substr(x, j, j))
      if (j == nchar(x))

        result <- latex2(result, brace_options(opts, start = letters[i] + 1))
      if (j == 1 && i > 2 && is_block(opts[[i-1]]) && !is_text(opts[[i-2]]))
        result <- insert_values(result, 1, opts[c(i-2, i-1)])
      break
    }
    start <- stop
  }
  result
}

#' @rdname tableOption
#' @examples
#' columnOptions(table) <- "lrr"
#' table
#' @export
`columnOptions<-` <- function(table,
                              asis = FALSE, value) {
  start <- 1
  which <- 1
  if (envName(table) %in% c("tabular*", "tabularx", "tabulary"))
    which <- 2
  if (envName(table) == "tabu") {
    while (!is_bracket(table[[start]], "[") &&
           !is_block(table[[start]])) start <- start + 1
  }
  brace_options(table, which = which, start = start, asis = asis) <- value
  table
}

#' @rdname tableOption
#' @examples
#' columnOption(table, 3) <- "p{1cm}"
#' columnOptions(table)
#' @export
`columnOption<-` <- function(table, column, value) {
  value <- latex2(value)
  letter <- find_tags(value, "TEXT")
  if (length(letter) != 1 || nchar(value[[letter]]) != 1)
    stop("unrecognized column specification")
  opts <- get_contents(columnOptions(table))
  start <- 0
  for (i in seq_along(opts)) {
    x <- opts[[i]]
    if (latexTag(x) == "TEXT") {
      stop <- start + nchar(x)
      if (column <= stop) {
        # We have the first entry.  Now things
        # depend on whether it has options, or value
        # has options.  Options like ">{3cm}" can appear
        # before, others like in "p{3cm}" after.
        # Vertical bars count as part of it only after.
        newarg <- length(value) > 1
        oldarg <- FALSE
        oldprearg <- NULL
        j <- column - start
        n <- nchar(x)
        if (j == n && i < length(opts) && is_block(opts[[i+1]]))
          oldarg <- TRUE
        if (j == 1 && i > 2 && is_block(opts[[i-1]]) && !is_text(opts[[i-2]]))
          oldprearg <- c(i-2, i-1)
        if (oldarg)
          opts <- drop_items(opts, i+1)
        if (newarg) {
          # Need to split up x to make room for new arg
          # NB:  substr copies the attributes from x to the
          #      substring
          if (j > 1)
            opts[[i]] <- substr(x, 1, j-1)
          if (j < n)
            opts <- insert_values(opts, i, substr(x, j+1, n), after = TRUE)
          opts <- insert_values(opts, i, value, after = TRUE)
          if (j == 1)
            opts <- drop_items(opts, i)
        } else
          substr(opts[[i]], j, j) <- value[[1]]
        if (!is.null(oldprearg))
          opts <- drop_items(opts, oldprearg)
        columnOptions(table) <- new_block(opts)
        return(table)
      }
      start <- stop
    }
  }
  stop("column ", column, " not found.")
}
