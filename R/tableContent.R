#' @title Functions relating to the data content of a table
#' @rdname tableContent
#' @param table A tabular-like environment to work with.  It
#' must not be one for which [prepare_table()] has been called.
#' @returns `find_tableContent()` returns the indices of the
#' entries corresponding to content of the table.
#'
#' @export
find_tableContent <- function(table) {
  if (has_itemlist(table))
    stop("this function does not work with itemlists.")

  skip <- find_columnOptions(table)
  if (length(skip))
    seq.int(skip$range + 1, length(table))
  else
    integer()
}

#' @rdname tableContent
#' @returns `tableContent()` returns a [LaTeX2] object containing
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
  as_LaTeX2(table[find_tableContent(table)])

#' @rdname tableContent
#' @param asis Should newlines be added around the
#' value?
#' @param value The content to be inserted into the cell.  This
#' can be a [LaTeX2] object, or a character string that will be
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
  has_itemlist <- has_itemlist(table)
  if (has_itemlist)
    table <- flatten_itemlists(table)
  i <- find_tableContent(table)
  table <- drop_items(table, i)
  table <- insert_values(table, min(i), value)
  if (has_itemlist)
    table <- prepare_table(table)
  table
}
