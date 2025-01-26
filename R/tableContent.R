#' @title Functions relating to the data content of a table
#' @rdname tableContent
#' @param table A tabular-like environment to work with.
#' @returns `find_tableContent()` returns the indices of the
#' entries corresponding to content of the table.
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

#' @rdname tableContent
#' @returns `tableContent()` returns a LaTeX2 object containing
#' all of the table content after the options.
#' @examples
#' latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
#' parsed <- parseLatex(latex)
#' table <- parsed[[find_tabular(parsed)]]
#' table
#' tableContent(table)
#'
#' @export
tableContent <- function(table)
  as_LaTeX2(table[[2]][find_tableContent(table)])

#' @rdname tableContent
#' @param asis Should newlines be added around the
#' value?
#' @param value The content to be inserted into the cell.  This
#' can be a LaTeX2 object, or a character string that will be
#' converted to one.
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
