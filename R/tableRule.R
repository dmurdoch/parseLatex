#' @title Work with rules in tables

#' @name tableRule
#' @returns `find_rules()` returns a list of [LaTeX2range]
#' objects giving the locations of the rules before each
#' line.  The last item in the list gives the location
#' of any rules after the last line.
#' @examples
#' latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
#' parsed <- parseLatex(latex)
#' table <- parsed[[find_tabular(parsed)]]
#' table <- prepare_table(table)
#' find_rules(table)
#'
#' @export
find_rules <- function(table) {

  if (!has_itemlist(table))
    stop("find_rules requires you to call prepare_table() first")

  result <- vector("list", length(table) - 1)

  for (i in 2:length(table)) {
    content <- table[[c(i, 1)]]
    rules <- find_macro(content, c("\\hline", "\\toprule", "\\midrule", "\\bottomrule"))

    # partial rules
    cline <- find_macro(content, "\\cline")
    if (length(cline))
      rules <- c(rules, cline, cline + 1)

    if (length(rules)) {
      rules <- include_whitespace(content, rules)
      result[[i - 1]] <- LaTeX2range(c(i, 1), min(rules):max(rules))
    }
  }
  result
}

#' @rdname tableRule
#' @param table A tabular-like environment to work with.
#' @param idx A list of indices as produced by `find_rules()`.
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
rules <- function(table, idx = find_rules(table)) {
  lapply(idx, function(x) get_range(table, x))
}

#' @rdname tableRule
#' @param row The rules will precede the contents of this row.
#' The rule after the final row uses `row = tableNrow(table) + 1`.
#' @returns `find_rule(table, row)` returns a [LaTeX2range]
#' for the rule before `row`, not including the final
#' whitespace.
#' @seealso Use [index_to_path()] to convert to a path.
#' @examples
#' find_rule(table, 1)
#'
#' @export
find_rule <- function(table, row) {
  res <- find_rules(table)[[row]]
  if (!is.null(res)) {
    path <- res$path
    range <- res$range
    while (length(range) && is_whitespace(table[[c(path, range[length(range)])]]))
      range <- range[-length(range)]
    if (length(range))
      res <- LaTeX2range(path, range)
    else
      res <- NULL
  }
  res
}

#' @rdname tableRule
#' @returns `rule(table, row)` returns the
#' rule(s) before `row`.
#' @examples
#' rule(table, 1)
#'
#' @export
rule <- function(table, row) {
  loc <- find_rule(table, row)
  if (!is.null(loc))
    get_range(table, loc)
  else
    NULL
}

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
`rule<-` <- function(table, row, asis = FALSE, idx = find_rules(table), value) {
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
