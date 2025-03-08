#' @title Functions to work with rows in tables
#' @rdname tableRow
#' @param table A tabular-like environment to work with.
#' @param row row in the table (1 is top row), including
#' rows of labels.
#' @returns `find_tableRow()` returns the indices of the
#' entries corresponding to the content of row i of the table.
#' @examples
#' latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
#' parsed <- parseLatex(latex)
#' table <- parsed[[find_tabular(parsed)]]
#' find_tableRow(table, 1)
#'
#' @export
find_tableRow <- function(table, row) {
  contentIdx <- find_tableContent(table)
  content <- as_LaTeX2(table[contentIdx])

  drop <- function(skip) {
    if (length(skip)) {
      skip <- unique(skip)
      contentIdx <<- contentIdx[-skip]
      content <<- as_LaTeX2(content[-skip])
    }
  }

  # Drop the captions
  idx <- find_captions(content)
  if (length(idx)) {
    idx <- unlist(attr(idx, "extra"))
    drop(idx)
  }

  # Drop the rules & \nopagebreak
  drop(find_macro(content, c("\\hline", "\\toprule", "\\midrule", "\\bottomrule")))

  # Drop pagebreak and nopagebreak
  idx <- find_macro(content, c("\\pagebreak", "\\nopagebreak"))
  if (length(idx)) {
    idx0 <- idx
    for (i in idx)
      idx0 <- c(idx0, find_bracket_options(content, start = idx[i]))
    drop(idx0)
  }

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

#' @rdname tableRow
#' @returns `tableRow()` returns a [LaTeX2] object containing
#' all of the table content in the row.
#' @examples
#' tableRow(table, 1)
#'
#' @export
tableRow <- function(table, row)
  as_LaTeX2(table[find_tableRow(table, row)])

blankRow <- function(table) {
  paste0(rep(" & ", tableNcol(table) - 1), collapse = "")
}

#' @rdname tableRow
#' @param asis Should a linebreak and newline be added after the
#' value?
#' @param value The content to be inserted into the cell.  This
#' can be a [LaTeX2] object, or a character string that will be
#' converted to one.
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
    content <- table[contentIdx]
    breaks <- contentIdx[find_macro(content, "\\\\")]
    blanks <- as_LaTeX2(c("", rep(paste0(blankRow(table), "\\\\"), row - length(breaks) - 1), ""))
    value <- c(blanks, value)
    if (length(breaks))
      i <- max(breaks) + 0.5
    else
      i <- 0
  }
  replace_range(table, i, value)
}

#' @title Convert vector to table row and back
#'
#' @param cells A list or vector of cell contents.
#' @param asis If `FALSE`, add or remove blanks around cell contents.
#' @param linebreak If `TRUE`, add a line break marker.
#' @returns `vector_to_row` returns a [LaTeX2] object which could be a row
#' in a tabular object.
#' @export
#'
#' @examples
#' vector_to_row(1:3)
vector_to_row <- function(cells, asis = FALSE, linebreak = TRUE) {
  result <- list()
  amp <- as_LaTeX2("&")
  eol <- as_LaTeX2("\\\\")
  blank <- as_LaTeX2(" ")
  for (i in seq_along(cells)) {
    result <- c(result,
                if (!asis && i > 1) blank,
                as_LaTeX2(cells[[i]]),
                if (!asis && i < length(cells)) blank,
                if (i == length(cells)) {
                  if (linebreak) eol
                } else amp)
  }
  as_LaTeX2(result)
}

#' @rdname vector_to_row
#'
#' @param row A row from a table
#' @returns `row_to_vector` returns a character vector of the
#' deparsed contents of the row, or if `deparse` is `FALSE`, a list of the contents.
#' @export
#'
#' @examples
#' row_to_vector("1 & 2 & content \\\\")
#' row_to_vector("1 & 2 & content \\\\", deparse = FALSE)
row_to_vector <- function(row, asis = FALSE, deparse = TRUE) {
  row <- as_LaTeX2(row)
  amp <- find_catcode(row, 4)
  eol <- find_macro(row, "\\\\")
  if (length(eol) > 1)
    warning("This row has a line break.  Only first line used")
  if (!length(eol))
    eol <- length(row) + 1
  br <- c(0, amp[amp < eol[1]], eol[1])
  if (deparse)
    result <- rep("", length(br) - 1)
  else
    result <- vector("list", length(br) - 1)
  for (i in seq_along(result))
    if (br[i+1] > br[i] + 1) {
      value <- row[(br[i] + 1):(br[i+1] - 1)]
      if (deparse)
        result[i] <- deparseLatex(value)
      else
        result[[i]] <- as_LaTeX2(value)
    }
  if (!asis) {
    if (deparse)
      result <- trimws(result)
    else
      result <- lapply(result, trim_whitespace)
  }
  result
}

#' @title Convert vector to items
#'
#' @param x A list or vector to convert.
#' @returns A [LaTeX2] object containing the entries
#' of `x` concatenated.
#' @export
#'
#' @examples
#' print(vector_to_latex2(1:3), tags = TRUE)
vector_to_latex2 <- function(x) {
  as_LaTeX2(do.call(c, lapply(x, as_LaTeX2)))
}
