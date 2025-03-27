#' Calculations on tables
#'
#' @name tablecalcs
#' @rdname tablecalcs
#' @param table A known tabular-like environment object.
#'
#' @returns `tableNrow()` returns the number of rows in the table.
#' @examples
#' latex <- kableExtra::kbl(mtcars[1:2, 1:3], format = "latex")
#' parsed <- parseLatex(latex)
#' table <- parsed[[find_tabular(parsed)]]
#' table
#' tableNrow(table)
#' @export
tableNrow <- function(table) {
  if (!has_itemlist(table))
    table <- prepare_table(table)
  length(table) - 2L
}

#' @rdname tablecalcs
#' @returns `tableNcol()` returns the number of columns in the table.
#' @examples
#' tableNcol(table)
#' @export
tableNcol <- function(table) {
  opts <- drop_whitespace(get_contents(columnOptions(table)))
  opts <- drop_items(opts, find_char(opts, "|"))
  sum(vapply(opts, function(x)
    if(latexTag(x) == "TEXT") nchar(x)
    else 0L, 0L))
}

#' @rdname tablecalcs
#' @returns `tableDim()` returns the number of rows and columns in the table.
#'
#' @examples
#' tableDim(table)
#' @export
tableDim <- function(table) {
  c(tableNrow(table), tableNcol(table))
}
