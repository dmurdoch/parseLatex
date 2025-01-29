#' @title Functions to work with rows in tables
#' @rdname tableRow
#' @param table A tabular-like environment to work with.
#' @param row row in the table (1 is top row), including
#' rows of labels.
#' @returns `find_tableRow()` returns the indices of the
#' entries corresponding to the content of row i of the table.
#' @examples
#' latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
#' parsed <- parseLatex(latex)
#' table <- parsed[[find_tabular(parsed)]]
#' find_tableRow(table, 1)
#'
#' @export
find_tableRow <- function(table, row) {
  contentIdx <- find_tableContent(table)
  content <- as_LaTeX2(table[contentIdx])

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

  # Drop partial rules
  cline <- find_macro(content, "\\cline")
  if (length(cline))
    drop(c(cline, cline + 1))

  # Drop all newlines
  drop(find_catcode(content, NEWLINE))

  breaks <- find_macro(content, "\\\\")
  rows <- split_list(contentIdx, breaks)
  if (row <= length(breaks))
    c(rows[[row]], contentIdx[breaks[row]])
  else if (row == length(rows))
    rows[[row]]
}

#' @rdname tableRow
#' @returns `tableRow()` returns a LaTeX2 object containing
#' all of the table content in the row.
#' @examples
#' tableRow(table, 1)
#'
#' @export
tableRow <- function(table, row)
  as_LaTeX2(table[find_tableRow(table, row)])

blankRow <- function(table) {
  paste0(rep(" & ", tableNcol(table) - 1), collapse = "")
}

#' @rdname tableRow
#' @param asis Should a linebreak and newline be added after the
#' value?
#' @param value The content to be inserted into the cell.  This
#' can be a LaTeX2 object, or a character string that will be
#' converted to one.
#' @details Unless `asis = TRUE`, `tableContent(table) <- value`
#'  will add "\\" and a newline
#' at the end if not present.
#'
#' If the `row` value is higher than the number of rows
#' in the table, blank rows will be added to fill the
#' space between.
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
    # if (!(length(value) %in% newlines))
    #   value <- c(value, as_LaTeX2("\n"))
  }
  i <- find_tableRow(table, row)
  if (!length(i)) {
    # Need to insert rows
    contentIdx <- find_tableContent(table)
    content <- table[contentIdx]
    breaks <- contentIdx[find_macro(content, "\\\\")]
    blanks <- as_LaTeX2(c("", rep(paste0(blankRow(table), "\\\\"), row - length(breaks) - 1), ""))
    value <- c(blanks, value)
    if (length(breaks))
      i <- max(breaks) + 0.5
    else
      i <- 0
  }
  replace_range(table, i, value)
}
