#' @title Functions to work with rows in tables
#' @rdname tableRow
#' @param table A tabular-like environment to work with.
#' @param row row in the table (1 is top row), including
#' rows of labels.
#' @param withExtras If `TRUE`, include the extras
#' before the line of data, such as `\hline`, etc.
#' @param withData If `TRUE`, include the data.
#' @returns `find_tableRow()` returns the indices of the
#' entries corresponding to the content of row i of the table.
#' @examples
#' latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
#' parsed <- parseLatex(latex)
#' table <- parsed[[find_tabular(parsed)]]
#' find_tableRow(table, 1)
#'
#' @export
find_tableRow <- function(table, row, withExtras = FALSE, withData = TRUE) {
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

  breaks <- contentIdx[find_macro(content, "\\\\")]
  if (row <= 0 || row > length(breaks) + 1)
    return(integer())

  if (!withExtras && !withData) {
    if (row > length(breaks))
      return(integer())
    else
      return(breaks[row ])
  }

  breaks <- c(0, breaks, Inf)

  # Drop stuff before and after the line we want
  idx <- which((contentIdx <= breaks[row]) |
               (contentIdx > breaks[row + 1]))
  drop(idx)

  if (withExtras && withData)
    return(contentIdx)

  # Find all the extras
  # The rules and addlinespace
  extras <- find_macro(content, c("\\hline", "\\toprule",
                               "\\midrule", "\\bottomrule",
                               "\\addlinespace"))

  # pagebreak and nopagebreak
  idx <- find_macro(content, c("\\pagebreak", "\\nopagebreak"))
  if (length(idx)) {
    idx0 <- idx
    for (i in idx)
      idx0 <- c(idx0, find_bracket_options(content, start = i + 1))
    extras <- c(extras, idx0)
  }

  # partial rules and rowcolor
  idx <- find_macro(content, c("\\cline", "\\rowcolor"))
  if (length(idx))
    extras <- c(extras, idx, idx + 1)

  # newlines
  extras <- c(extras, find_catcode(content, NEWLINE))

  if (withExtras)   # just extras...
    contentIdx[sort(unique(extras))]
  else {            # just data
    drop(extras)
    contentIdx
  }
}

#' @rdname tableRow
#' @returns `tableRow()` returns a [LaTeX2] object containing
#' all of the table content in the row.
#' @examples
#' tableRow(table, 1)
#' tableRow(table, 1, withExtras = TRUE)
#'
#' @export
tableRow <- function(table, row, withExtras = FALSE, withData = TRUE)
  as_LaTeX2(table[find_tableRow(table, row, withExtras, withData)])

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
#'
#' If `withExtras = TRUE` and you want the
#' result to start on a new line, you need to add the
#' newline explicitly in `value` when using the
#' assignment function.
#' @examples
#' tableRow(table, 5) <- "a & b & c"
#' table
#'
#' @export
`tableRow<-` <- function(table, row, asis = FALSE,
                         withExtras = FALSE,
                         withData = TRUE,
                         value) {
  value <- as_LaTeX2(value)
  if (!asis) {
    breaks <- find_macro(value, "\\\\")
    if (!length(breaks))
      value <- c(value, as_LaTeX2("\\\\"))
    newlines <- find_catcode(value, NEWLINE)
    # if (!(length(value) %in% newlines))
    #   value <- c(value, as_LaTeX2("\n"))
  }
  i <- find_tableRow(table, row, withExtras = withExtras, withData = withData)
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
#' @param deparse Should the result be deparsed?
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
