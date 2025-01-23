
#' @title Functions related to parsing LaTeX tables
#' @name tables
#' @rdname tables
#' @param item An item from a LaTeX2 list object.
#' @returns `is_Tabular()` returns boolean indicating if this is
#' a tabular-like environment.
#' @examples
#' latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
#' parsed <- parseLatex(latex)
#' is_Tabular(parsed[[2]])
#'
#'
#' @export
is_Tabular <- function(item) {
  name <- envName(item)
  length(name) == 1 &&
    name %in% c("tabular", "tabular*", "tabularx", "tabulary",
                       "longtable")
}

#' @rdname tables
#' @param items A LaTeX2 list object.
#' @param start Where to start looking
#' @returns `find_tabular()` returns the index of the first
#' tabular-like environment, or `NA` if none is found
#' @examples
#' find_tabular(parsed)
#' table <- parsed[[find_tabular(parsed)]]
#' table
#'
#' @export
find_tabular <- function(items, start = 1) {
  if (start <= length(items))
    for (i in seq_along(items))
      if (is_Tabular(items[[i]]))
        return(i)
  NA
}

#' @rdname tables
#' @param table A known tabular-like environment object.
#' @returns `find_posOption()` returns the indices of the
#' entries corresponding to the "pos" option, including the
#' brackets, within the contents list (i.e. `table[[2]]`).
#' @examples
#' find_posOption(table)
#' table[[2]][find_posOption(table)]
#'
#' @export
find_posOption <- function(table) {
  stopifnot(is_Tabular(table))
  find_bracket_options(table[[2]])
}

#' @rdname tables
#' @returns `posOption()` returns a LaTeX2 object containing the
#' "pos" option.
#' @examples
#' posOption(table)
#'
#' @export
posOption <- function(table)
  as_LaTeX2(table[[2]][find_posOption(table)])

#' @param value A character string or LaTeX2 object
#' @param asis Whether to make small modifications in replacement functions
#' @details Unless `asis == TRUE`, the value for `value` in `posOption(table) <- value`
#' can be specified with or without the enclosing brackets.
#' @rdname tables

#' @examples
#' posOption(table) <- "h"
#' posOption(table)
#' @export
`posOption<-` <- function(table, asis = FALSE, value) {
  value <- as_LaTeX2(value)
  if (!asis && !is_bracket(value[[1]], "["))
    value <- parseLatex(paste0("[", as.character(value), "]"))
  i <- find_posOption(table)
  if (!length(i))
    i <- 0
  old <- table[[2]]
  iold <- seq_along(old)
  table[[2]] <- c(old[iold < min(i)],
                  value,
                  old[iold > max(i)])
  table
}

#' @rdname tables
#' @returns `find_widthOption()` returns the index of the
#' block corresponding to the "width" option, if there is one.
#' Only some tabular-like environments have these.
#' @examples
#' find_widthOption(table)
#'
#' @export
find_widthOption <- function(table) {
  stopifnot(is_Tabular(table))
  if (envName(table) %in% c("tabular*", "tabularx", "tabulary"))
    find_brace_options(table[[2]])
}

#' @rdname tables
#' @returns `widthOption()` returns a LaTeX2 object containing the
#' "width" option, if the table has one.
#' @examples
#' widthOption(table)
#'
#' @export
widthOption <- function(table)
  as_LaTeX2(table[[2]][find_widthOption(table)])

#' @rdname tables
#' @export
`widthOption<-` <- function(table, asis = FALSE, value) {
  if (envName(table) %in% c("tabular", "longtable")) {
    warning("tables of type ", dQuote(envName(table)),
            " do not support a width option.  No change made.")
    return(table)
  }
  value <- as_LaTeX2(value)
  if (!asis && (length(value) > 1 || !is_block(value[[1]])))
    value <- parseLatex(paste0("{", as.character(value), "}"))
  # Need to figure out where to put it.
  i <- find_widthOption(table)
  if (!length(i))
    i <- find_columnOptions(table) - 0.5
  if (!length(i)) {
    i <- find_posOption(table)
    if (length(i)) i <- max(i) + 0.5
  }
  if (!length(i))
    i <- 0

  old <- table[[2]]
  iold <- seq_along(old)
  table[[2]] <- c(old[iold < i],
                  value,
                  old[iold > i])
  table
}

#' @rdname tables
#' @returns `find_columnOptions()` returns the index of the
#' block corresponding to the column spec.
#' @examples
#' find_columnOptions(table)
#' @export
find_columnOptions <- function(table) {
  stopifnot(is_Tabular(table))
  which <- 1
  if (envName(table) %in% c("tabular*", "tabularx", "tabulary"))
    which <- 2
  find_brace_options(table[[2]], which = which)
}

#' @rdname tables
#' @returns `columnOptions()` returns a LaTeX2 object containing the
#' "columb" options.
#' @examples
#' columnOptions(table)
#'
#' @export
columnOptions <- function(table)
  as_LaTeX2(table[[2]][find_columnOptions(table)])

#' @rdname tables
#' @examples
#' columnOptions(table) <- "lrr"
#' table
#' @export
`columnOptions<-` <- function(table, asis = FALSE, value) {
  value <- as_LaTeX2(value)
  if (!asis && (length(value) > 1 || !is_block(value[[1]])))
    value <- parseLatex(paste0("{", as.character(value), "}"))
  i <- find_columnOptions(table)
  if (!length(i))
    i <- find_widthOption(table) + 0.5
  if (!length(i)) {
    i <- find_posOption(table)
    if (length(i)) i <- max(i) + 0.5
  }
  if (!length(i))
    i <- 0

  old <- table[[2]]
  iold <- seq_along(old)
  table[[2]] <- c(old[iold < i],
                  value,
                  old[iold > i])
  table
}

#' @rdname tables
#' @returns `find_tableContent()` returns the indices of the
#' entries corresponding to content of the table.
#' @examples
#' find_tableContent(table)
#'
#' @export
find_tableContent <- function(table) {
  skip <- length(find_posOption(table)) +
          length(find_widthOption(table)) +
          length(find_columnOptions(table))
  if (skip < length(table[[2]]))
    seq.int(skip + 1, length(table[[2]]))
  else
    integer()
}

#' @rdname tables
#' @returns `tableContent()` returns a LaTeX2 object containing
#' all of the table content after the options.
#' @examples
#' tableContent(table)
#'
#' @export
tableContent <- function(table)
  as_LaTeX2(table[[2]][find_tableContent(table)])

#' @rdname tables
#' @details Unless `asis = TRUE`, `tableContent(table) <- value`
#'  will add newlines
#' at the start end end if not present, to make the result
#' more readable.
#' @examples
#' tableContent(table) <- "Mazda RX4 & 21 & 6\\\\"
#' table
#' tableContent(table, asis = TRUE) <- "Mazda RX4 & 21 & 6\\\\"
#' table
#'
#' @export
`tableContent<-` <- function(table, asis = FALSE, value) {
  value <- as_LaTeX2(value)
  if (!asis) {
    newlines <- find_catcode(value, NEWLINE)
    if (!(length(value) %in% newlines))
      value <- c(value, as_LaTeX2("\n"))
    if (!(1 %in% newlines))
      value <- c(as_LaTeX2("\n"), value)
  }
  i <- find_tableContent(table)
  old <- table[[2]]
  iold <- seq_along(old)
  table[[2]] <- c(old[iold < min(i)],
                  value,
                  old[iold > max(i)])
  table
}

#' @rdname tables
#' @param row row in the table (1 is top row), including
#' rows of labels
#' @returns `find_tableRow()` returns the indices of the
#' entries corresponding to the content of row i of the table.
#' @examples
#' find_tableRow(table, 1)
#'
#' @export
find_tableRow <- function(table, row) {
  contentIdx <- find_tableContent(table)
  content <- as_LaTeX2(table[[2]][contentIdx])

  drop <- function(skip) {
    if (length(skip)) {
      skip <- unique(skip)
      contentIdx <<- contentIdx[-skip]
      content <<- as_LaTeX2(content[-skip])
    }
  }

  # Drop the captions
  skip <- find_macro(content, c("\\caption", "\\caption*"))
  drop(c(skip, skip + 1))

  # Drop the rules
  drop(find_macro(content, c("\\hline", "\\toprule", "\\midrule", "\\bottomrule")))

  # Drop all newlines
  drop(find_catcode(content, NEWLINE))

  breaks <- find_macro(content, "\\\\")
  rows <- split_list(contentIdx, breaks)
  if (row <= length(rows))
    c(rows[[row]], breaks[row])
}

#' @rdname tables
#' @returns `tableRow()` returns a LaTeX2 object containing
#' all of the table content in the row
#' @examples
#' tableRow(table, 1)
#'
#' @export
tableRow <- function(table, row)
  as_LaTeX2(table[[2]][find_tableRow(table, row)])

#' @rdname tables
#' @details Unless `asis = TRUE`, `tableContent(table) <- value`
#'  will add \\ and a newline
#' at the end if not present.
#' @examples
#' tableRow(table, 5) <- "a & b & c"
#' table
#'
#' @export
`tableRow<-` <- function(table, row, asis = FALSE, value) {
  value <- as_LaTeX2(value)
  if (!asis) {
    breaks <- find_macro(value, "\\\\")
    if (!length(breaks))
      value <- c(value, as_LaTeX2("\\\\"))
    newlines <- find_catcode(value, NEWLINE)
    if (!(length(value) %in% newlines))
      value <- c(value, as_LaTeX2("\n"))
  }
  i <- find_tableRow(table, row)
  if (!length(i)) {
    # Need to insert rows
    contentIdx <- find_tableContent(table)
    content <- table[[2]][contentIdx]
    breaks <- contentIdx[find_macro(content, "\\\\")]
    brk <- as_LaTeX2("\\\\\n")
    value <- c(rep(brk, row - length(breaks) - 1),
               value)
    if (length(breaks))
      i <- max(breaks) + 0.5
    else
      i <- 0
  }
  old <- table[[2]]
  iold <- seq_along(old)
  table[[2]] <- c(old[iold < min(i)],
                  value,
                  old[iold > max(i)])
  table
}

#' @rdname tables
#' @param col column in the table
#' @returns `find_tableCell()` returns the indices of the
#' entries corresponding to the content of the cell (row, col) of the table.
#' @examples
#' find_tableCell(table, 1, 2)
#'
#' @export
find_tableCell <- function(table, row, col) {
  contentIdx <- find_tableRow(table, row)
  content <- as_LaTeX2(table[[2]][contentIdx])
  if (is_macro(content[[length(content)]], "\\\\")) {
    content <- content[-length(content)]
    contentIdx <- contentIdx[-length(content)]
  }
  breaks <- find_catcode(content, ALIGN)
  cells <- split_list(contentIdx, breaks)
  if (col <= length(cells))
    cells[[col]]
}

#' @rdname tables
#' @returns `tableCell()` returns a LaTeX2 object containing
#' all of the table content in the cell (but not the &)
#' @examples
#' tableCell(table, 1, 2)
#'
#' @export
tableCell <- function(table, row, col)
  as_LaTeX2(table[[2]][find_tableCell(table, row, col)])

#' @rdname tables
#' @details Unless `asis = TRUE`, `tableContent(table) <- value`
#'  will add blanks
#' at the start end end if not present, to make the result
#' more readable.
#' @examples
#' tableCell(table, 5, 2) <- " d "
#' table
#'
#' @export
`tableCell<-` <- function(table, row, col, asis = FALSE, value) {
  value <- as_LaTeX2(value)
  if (!asis) {
    blanks <- find_catcode(value, SPACE)
    if (!(length(value) %in% blanks))
      value <- c(value, as_LaTeX2(" "))
    if (!(1 %in% blanks))
      value <- c(as_LaTeX2(" "), value)
  }
  i <- find_tableCell(table, row, col)
  if (!length(i))
    stop("Can't add cells that are not present.")
  old <- table[[2]]
  iold <- seq_along(old)
  table[[2]] <- c(old[iold < min(i)],
                  value,
                  old[iold > max(i)])
  table
}
