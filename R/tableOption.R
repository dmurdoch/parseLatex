#' @rdname tableOption
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
  find_bracket_options(table)
}

#' @rdname tableOption
#' @returns `posOption()` returns a LaTeX2 object containing the
#' "pos" option.
#' @examples
#' posOption(table)
#'
#' @export
posOption <- function(table) {
  as_LaTeX2(table[find_posOption(table)])
}

#' @param value A character string or LaTeX2 object.
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
#' @returns `widthOption()` returns a LaTeX2 object containing the
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
#' @returns `find_columnOptions()` returns the index of the
#' block corresponding to the column spec.
#' @examples
#' find_columnOptions(table)
#' @export
find_columnOptions <- function(table) {
  which <- 1
  if (envName(table) %in% c("tabular*", "tabularx", "tabulary"))
    which <- 2
  find_brace_options(table, which = which)
}

#' @rdname tableOption
#' @returns `columnOptions()` returns a LaTeX2 object containing the
#' "column" options.
#' @examples
#' columnOptions(table)
#'
#' @export
columnOptions <- function(table) {
  as_LaTeX2(table[find_columnOptions(table)])
}

#' @rdname tableOption
#' @examples
#' columnOptions(table) <- "lrr"
#' table
#' @export
`columnOptions<-` <- function(table,
                              asis = FALSE, value) {
  which <- 1
  if (envName(table) %in% c("tabular*", "tabularx", "tabulary"))
    which <- 2
  brace_options(table, which = which, asis = asis) <- value
  table
}
