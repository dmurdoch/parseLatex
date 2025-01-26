
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
#' @param start Where to start looking.
#' @returns `find_tabular()` returns the index of the first
#' tabular-like environment, or `NA` if none is found.
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

