
#' @title Work with table cells
#' @name tableCell
#' @param table A tabular-like environment to work with.
#' @param row,col row and column in the table.
#' @description
#' These functions work with the content of cells in
#' tabular-like environments.  Cells are numbered with the first
#' row (typically column titles) being row 1.  Rules (i.e.
#' horizontal lines) are not considered part of a cell.
#' @returns `find_tableCell()` returns the indices of the
#' entries corresponding to the content of the cell (row, col) of the table.
#' @details `find_tableCell()` returns `NA` if the requested
#' cell is missing because an earlier cell covered multiple
#' columns.  It signals an error if a request is made beyond
#' the bounds of the table.
#' @examples
#' latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
#' parsed <- parseLatex(latex)
#' table <- split_table(parsed[[find_tabular(parsed)]])
#' find_tableCell(table, 1, 2)
#'
#' @export
find_tableCell <- function(table, row, col) {
  rowidx <- list_idx(table)
  if (is.null(rowidx))
    stop("table row operations require a previous call to split_table()")
  rows <- table[[rowidx]]
  if (row > length(rows))
    stop("row is beyond the end of the table")
  therow <- rows[[row]]
  colidx <- list_idx(therow)
  if (is.null(colidx))
    stop("table cell operations require a previous call to split_row()")
  LaTeX2range(c(rowidx, row, colidx, col), range = NULL)
}

#' @rdname tableCell
#' @returns `tableCell()` returns a [LaTeX2] object containing
#' all of the table content in the cell (but not the &).
#' @examples
#' tableCell(table, 1, 2)
#'
#' @export
tableCell <- function(table, row, col) {
  entries <- find_tableCell(table, row, col)
  if (is.na(entries$path))
    warning("Cell is missing because of earlier \\multicolumn cell.")
  else
    as_LaTeX2(get_range(table, entries))
}

#' @rdname tableCell
#' @param asis Should blanks be added around the value?
#' @param value The content to be inserted into the cell.  This
#' can be a [LaTeX2] object, or a character string that will be
#' converted to one.
#' @details Unless `asis = TRUE`, `tableContent(table) <- value`
#'  will add blanks
#' at the start end end if not present, to make the result
#' more readable.
#'
#' If `col` is higher than the current table width,
#' the assignment will fail with an error.  If only `row`
#' is too high, blank lines will be added and it should
#' succeed.
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
  if (row > tableNrow(table))
    tableRow(table, row) <- blankRow(table)
  entries <- find_tableCell(table, row, col)
  if (is.na(entries))
    stop("Can't add cell covered by earlier \\multicolumn cell.")
  if (is.null(entries))
    stop("Cell not found.")
  set_range(entries, new_itemlist(value))
}

# this expands multicolumn macros to
# an equivalent number of columns.  The
# index of the added stuff is NA

expandMulticolumn <- function(itemlist) {
  if (!is_itemlist(itemlist))
    stop("this needs to be an itemlist")

  for (idx in rev(seq_along(itemlist))) {
    items <- itemlist[[idx]]
    multis <- find_macro(items, "\\multicolumn")

    if (length(multis) > 1)
      stop("cell has ", length(multis), " \\multicolumn macros")
    if (length(multis)) {
      whitespace <- find_whitespace(items)
      args <- setdiff(multis + seq_len(length(items) - multis), whitespace)
      if (length(args) < 3)
        stop("Badly formed \\multicolumn")
      args <- args[1:3]
      arg <- args[1]
      if (is_block(items[[arg]]))
        count <- as.numeric(deparseLatex(get_contents(items[arg])))
      else
        count <- as.numeric(items[[arg]])
      if (count > 1) {
        # add some placeholders
        for (j in seq_len(count - 1))
          itemlist <- insert_values(itemlist, idx + 1, placeholder())
      }
    }
  }
  itemlist
}

