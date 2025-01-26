#' @title Work with table cells
#' @rdname tablecell
#' @param table A tabular-like environment to work with.
#' @param row,col row and column in the table
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
#' table <- parsed[[find_tabular(parsed)]]
#' find_tableCell(table, 1, 2)
#'
#' @export
find_tableCell <- function(table, row, col) {
  contentIdx <- find_tableRow(table, row)
  if (is.null(contentIdx))
    stop(sprintf("row %d is too high", row))
  content <- as_LaTeX2(table[[2]][contentIdx])

  fix <- expandMulticolumn(content, contentIdx)
  content <- fix[[1]]
  contentIdx <- fix[[2]]

  terminated <- is_macro(content[[length(content)]],
                         "\\\\")
  breaks <- c(find_catcode(content, ALIGN),
              if (terminated) length(content))
  cells <- split_list(contentIdx, breaks)
  if (terminated)
    cells[[length(cells)]] <- NULL

  if (col <= length(cells))
    result <- cells[[col]]
  else
    stop(sprintf("col %d is too high", col))

  result
}

#' @rdname tablecell
#' @returns `tableCell()` returns a LaTeX2 object containing
#' all of the table content in the cell (but not the &)
#' @examples
#' tableCell(table, 1, 2)
#'
#' @export
tableCell <- function(table, row, col) {
  entries <- find_tableCell(table, row, col)
  if (any(is.na(entries)))
    warning("Cell is missing because of earlier \\multicolumn cell.")
  else
    as_LaTeX2(table[[2]][entries])
}

#' @rdname tablecell
#' @param asis Should blanks be added around the value?
#' @param value The content to be inserted into the cell.  This
#' can be a LaTeX2 object, or a character string that will be
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
  i <- find_tableCell(table, row, col)
  if (any(is.na(i)))
    stop("Can't add cell covered by earlier \\multicolumn cell.")
  if (!length(i)) {
    i <- find_tableRow(table, row)
    content <- table[[2]][i]
    fix <- expandMulticolumn(content, i)
    content <- fix[[1]]
    i <- fix[[2]]
    terminated <- is_macro(content[[length(content)]],
                           "\\\\")
    breaks <- c(find_catcode(content, ALIGN),
                if (terminated) length(content))
    i <- i[breaks[col]] - 0.5
  }

  old <- table[[2]]
  iold <- seq_along(old)
  table[[2]] <- c(old[iold < min(i)],
                  value,
                  old[iold > max(i)])
  table
}

# this expands multicolumn macros to
# an equivalent number of columns.  The
# index of the added stuff is NA

expandMulticolumn <- function(items, idx) {
  multis <- find_macro(items, "\\multicolumn")
  if (length(multis)) {
    whitespace <- find_whitespace(items)
    for (i in rev(seq_along(multis))) {
      multi <- multis[i]
      args <- setdiff(multi + seq_len(length(items) - multi), whitespace)
      if (length(args) < 3)
        stop("Badly formed \\multicolumn")
      args <- args[1:3]
      arg <- args[1]
      if (latexTag(items[[arg]]) == "BLOCK")
        count <- as.numeric(items[arg][[1]])
      else
        count <- as.numeric(items[arg])
      if (count > 1) {
        # add some fake alignment markers
        markers <- rep(as_LaTeX2("& "), count - 1)
        i <- seq_along(items)
        items <- as_LaTeX2(c(items[i <= args[3]],
                             markers,
                             items[i > args[3]]))
        idx <- c(idx[i <= args[3]],
                 rep(NA_integer_, length(markers)),
                 idx[i > args[3]])
      }
    }
  }
  list(items, idx)
}

