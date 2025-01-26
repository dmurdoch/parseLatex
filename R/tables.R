
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
#' @param start Where to start looking
#' @returns `find_tabular()` returns the index of the first
#' tabular-like environment, or `NA` if none is found
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

#' @rdname tables
#' @param table A known tabular-like environment object.
#' @returns `find_posOption()` returns the indices of the
#' entries corresponding to the "pos" option, including the
#' brackets, within the contents list (i.e. `table[[2]]`).
#' @examples
#' find_posOption(table)
#' table[[2]][find_posOption(table)]
#'
#' @export
find_posOption <- function(table) {
  stopifnot(is_Tabular(table))
  find_bracket_options(table[[2]])
}

#' @rdname tables
#' @returns `posOption()` returns a LaTeX2 object containing the
#' "pos" option.
#' @examples
#' posOption(table)
#'
#' @export
posOption <- function(table)
  as_LaTeX2(table[[2]][find_posOption(table)])

#' @param value A character string or LaTeX2 object
#' @param asis Whether to make small modifications in replacement functions
#' @details Unless `asis == TRUE`, the value for `value` in `posOption(table) <- value`
#' can be specified with or without the enclosing brackets.
#' @rdname tables

#' @examples
#' posOption(table) <- "h"
#' posOption(table)
#' @export
`posOption<-` <- function(table, asis = FALSE, value) {
  value <- as_LaTeX2(value)
  if (!asis && !is_bracket(value[[1]], "["))
    value <- parseLatex(paste0("[", as.character(value), "]"))
  i <- find_posOption(table)
  if (!length(i))
    i <- 0
  old <- table[[2]]
  iold <- seq_along(old)
  table[[2]] <- c(old[iold < min(i)],
                  value,
                  old[iold > max(i)])
  table
}

#' @rdname tables
#' @returns `find_widthOption()` returns the index of the
#' block corresponding to the "width" option, if there is one.
#' Only some tabular-like environments have these.
#' @examples
#' find_widthOption(table)
#'
#' @export
find_widthOption <- function(table) {
  stopifnot(is_Tabular(table))
  if (envName(table) %in% c("tabular*", "tabularx", "tabulary"))
    find_brace_options(table[[2]])
}

#' @rdname tables
#' @returns `widthOption()` returns a LaTeX2 object containing the
#' "width" option, if the table has one.
#' @examples
#' widthOption(table)
#'
#' @export
widthOption <- function(table)
  as_LaTeX2(table[[2]][find_widthOption(table)])

#' @rdname tables
#' @export
`widthOption<-` <- function(table, asis = FALSE, value) {
  if (envName(table) %in% c("tabular", "longtable")) {
    warning("tables of type ", dQuote(envName(table)),
            " do not support a width option.  No change made.")
    return(table)
  }
  value <- as_LaTeX2(value)
  if (!asis && (length(value) > 1 || !is_block(value[[1]])))
    value <- parseLatex(paste0("{", as.character(value), "}"))
  # Need to figure out where to put it.
  i <- find_widthOption(table)
  if (!length(i))
    i <- find_columnOptions(table) - 0.5
  if (!length(i)) {
    i <- find_posOption(table)
    if (length(i)) i <- max(i) + 0.5
  }
  if (!length(i))
    i <- 0

  old <- table[[2]]
  iold <- seq_along(old)
  table[[2]] <- c(old[iold < i],
                  value,
                  old[iold > i])
  table
}

#' @rdname tables
#' @returns `find_columnOptions()` returns the index of the
#' block corresponding to the column spec.
#' @examples
#' find_columnOptions(table)
#' @export
find_columnOptions <- function(table) {
  stopifnot(is_Tabular(table))
  which <- 1
  if (envName(table) %in% c("tabular*", "tabularx", "tabulary"))
    which <- 2
  find_brace_options(table[[2]], which = which)
}

#' @rdname tables
#' @returns `columnOptions()` returns a LaTeX2 object containing the
#' "columb" options.
#' @examples
#' columnOptions(table)
#'
#' @export
columnOptions <- function(table)
  as_LaTeX2(table[[2]][find_columnOptions(table)])

#' @rdname tables
#' @examples
#' columnOptions(table) <- "lrr"
#' table
#' @export
`columnOptions<-` <- function(table, asis = FALSE, value) {
  value <- as_LaTeX2(value)
  if (!asis && (length(value) > 1 || !is_block(value[[1]])))
    value <- parseLatex(paste0("{", as.character(value), "}"))
  i <- find_columnOptions(table)
  if (!length(i))
    i <- find_widthOption(table) + 0.5
  if (!length(i)) {
    i <- find_posOption(table)
    if (length(i)) i <- max(i) + 0.5
  }
  if (!length(i))
    i <- 0

  old <- table[[2]]
  iold <- seq_along(old)
  table[[2]] <- c(old[iold < i],
                  value,
                  old[iold > i])
  table
}

#' @rdname tables
#' @returns `find_tableContent()` returns the indices of the
#' entries corresponding to content of the table.
#' @examples
#' find_tableContent(table)
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

#' @rdname tables
#' @returns `tableContent()` returns a LaTeX2 object containing
#' all of the table content after the options.
#' @examples
#' tableContent(table)
#'
#' @export
tableContent <- function(table)
  as_LaTeX2(table[[2]][find_tableContent(table)])

#' @rdname tables
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

#' @rdname tables
#' @returns `find_rules()` returns a list of the indices
#' of rules before each row, including the whitespace
#' following each one
#' @examples
#' find_rules(table)
#'
#' @export
find_rules <- function(table) {
  contentIdx <- find_tableContent(table)
  content <- as_LaTeX2(table[[2]][contentIdx])

  # linebreaks
  breaks <- find_macro(content, "\\\\")

  split <- split_list(contentIdx, breaks)

  rules <- find_macro(content, c("\\hline", "\\toprule", "\\midrule", "\\bottomrule"))

  # partial rules
  cline <- find_macro(content, "\\cline")
  if (length(cline))
    rules <- c(rules, cline, cline + 1)

  rules <- include_whitespace(content, rules)

  lapply(split, function(x) contentIdx[intersect(x, rules)])
}

#' @rdname tables
#' @returns `rules(table)` returns a list of the rules
#' before each row.  The last entry will be the rule(s)
#' following the last row.
#' @examples
#' rules(table)
#'
#' @export
rules <- function(table) {
  idx <- find_rules(table)
  lapply(idx, function(x) as_LaTeX2(table[[2]][x]))
}

#' @rdname tables
#' @returns `find_rule(table, row)` returns the indices
#' of the rule(s) before `row`.
#' @examples
#' find_rule(table, 1)
#'
#' @export
find_rule <- function(table, row)
  find_rules(table)[[row]]

#' @rdname tables
#' @returns `rule(table, row)` returns the indices
#' rule(s) before `row`.
#' @examples
#' rule(table, 1)
#'
#' @export
rule <- function(table, row)
  as_LaTeX2(table[[2]][find_rule(table, row)])

#' @rdname tables
#' @examples
#' rule(table, 2) <- "\\midrule"
#'
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
  old <- table[[2]]
  iold <- seq_along(old)
  table[[2]] <- c(old[iold < min(i)],
                  value,
                  old[iold > max(i)])
  table
}

#' @rdname tables
#' @param row row in the table (1 is top row), including
#' rows of labels
#' @returns `find_tableRow()` returns the indices of the
#' entries corresponding to the content of row i of the table.
#' @examples
#' find_tableRow(table, 1)
#'
#' @export
find_tableRow <- function(table, row) {
  contentIdx <- find_tableContent(table)
  content <- as_LaTeX2(table[[2]][contentIdx])

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

#' @rdname tables
#' @returns `tableRow()` returns a LaTeX2 object containing
#' all of the table content in the row
#' @examples
#' tableRow(table, 1)
#'
#' @export
tableRow <- function(table, row)
  as_LaTeX2(table[[2]][find_tableRow(table, row)])

blankRow <- function(table) {
  paste0(rep(" & ", tableNcol(table) - 1), collapse = "")
}

#' @rdname tables
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
    content <- table[[2]][contentIdx]
    breaks <- contentIdx[find_macro(content, "\\\\")]
    blanks <- as_LaTeX2(c("", rep(paste0(blankRow(table), "\\\\"), row - length(breaks) - 1), ""))
    value <- c(blanks, value)
    if (length(breaks))
      i <- max(breaks) + 0.5
    else
      i <- 0
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

#' @rdname tables
#' @param col column in the table
#' @returns `find_tableCell()` returns the indices of the
#' entries corresponding to the content of the cell (row, col) of the table.
#' @details `find_tableCell()` returns `NA` if the requested
#' cell is missing because an earlier cell covered multiple
#' columns.  It signals an error if a request is made beyond
#' the bounds of the table.
#' @examples
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

#' @rdname tables
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

#' @rdname tables
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
