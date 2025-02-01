#' @title Work with rules in tables

#' @name tableRule
#' @returns `find_rules()` returns a list of the indices
#' of rules before each row, including the whitespace
#' following each one.
#' @examples
#' latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
#' parsed <- parseLatex(latex)
#' table <- parsed[[find_tabular(parsed)]]
#' table
#' find_rules(table)
#'
#' @export
find_rules <- function(table) {
  contentIdx <- find_tableContent(table)
  content <- as_LaTeX2(table[contentIdx])

  # linebreaks
  breaks <- find_macro(content, "\\\\")

  split <- split_list(contentIdx, breaks)

  rules <- find_macro(content, c("\\hline", "\\toprule", "\\midrule", "\\bottomrule"))

  # partial rules
  cline <- find_macro(content, "\\cline")
  if (length(cline))
    rules <- c(rules, cline, cline + 1)

  rules <- include_whitespace(content, rules)

  lapply(split, function(x) intersect(x, contentIdx[rules]))
}

#' @rdname tableRule
#' @param table A tabular-like environment to work with.
#' @description
#' In LaTeX, "rules" are horizontal lines in a table.
#' These functions let rules be extracted or modified.
#' @returns `rules(table)` returns a list of the rules
#' before each row.  The last entry will be the rule(s)
#' following the last row.
#' @examples
#' rules(table)
#'
#' @export
rules <- function(table) {
  idx <- find_rules(table)
  lapply(idx, function(x) as_LaTeX2(table[x]))
}

#' @rdname tableRule
#' @param row The rules will precede the contents of this row.
#' The rule after the final row uses `row = tableNrow(table) + 1`.
#' @returns `find_rule(table, row)` returns the indices
#' of the rule(s) before `row`.
#' @examples
#' find_rule(table, 1)
#'
#' @export
find_rule <- function(table, row)
  find_rules(table)[[row]]

#' @rdname tableRule
#' @returns `rule(table, row)` returns the indices
#' rule(s) before `row`.
#' @examples
#' rule(table, 1)
#'
#' @export
rule <- function(table, row)
  as_LaTeX2(table[find_rule(table, row)])

#' @rdname tableRule
#' @param asis Should a newline be added after the
#' value?  If `asis = TRUE`, it will not be.
#' @param value The content to be inserted into the cell.  This
#' can be a [LaTeX2] object, or a character string that will be
#' converted to one.
#' @examples
#' rule(table, 2) <- "\\midrule"
#' table
#' @export
`rule<-` <- function(table, row, asis = FALSE, value) {
  value <- as_LaTeX2(value)
  if (!asis) {
    breaks <- find_macro(value, "\\\\")
    newlines <- find_catcode(value, NEWLINE)
    if (!(length(value) %in% newlines))
      value <- c(value, as_LaTeX2("\n"))
  }
  i <- find_rule(table, row)
  if (!length(i)) {
    # Need to insert new rule at start of row
    rowidx <- find_tableRow(table, row)
    if (length(rowidx))
      i <- min(rowidx) - 0.5
    else {
      if (row > tableNrow(table) + 1)
        # need to insert some blank rows
        tableRow(table, row - 1) <- blankRow(table)
      rowidx <- find_tableRow(table, row - 1)
      i <- max(rowidx) + 0.5
    }
  }
  replace_range(table, i, value)
}
