find_extras <- function(row) {
  # Find all the extras
  # The rules and addlinespace
  extras <- find_macro(row, c("\\hline", "\\toprule",
                              "\\midrule", "\\bottomrule",
                              "\\addlinespace"))

  # pagebreak and nopagebreak
  idx <- find_macro(row, c("\\pagebreak", "\\nopagebreak"))
  if (length(idx)) {
    idx0 <- idx
    for (i in idx)
      idx0 <- c(idx0, find_bracket_options(row, start = i + 1))
    extras <- c(extras, idx0)
  }

  # partial rules and rowcolor
  idx <- find_macro(row, c("\\cline", "\\rowcolor"))
  if (length(idx))
    extras <- c(extras, idx, idx + 1)

  # cmidrule
  idx <- find_macro(row, "\\cmidrule")
  if (length(idx)) {
    for (i in idx) {
      i2 <- find_block(row[(i+1):length(row)], all= FALSE)
      extras <- c(extras, i + 0:i2)
    }
  }

  # newlines
  extras <- c(extras, find_catcode(row, NEWLINE))
  sort(unique(extras))
}

#' @title Functions to work with rows in tables
#' @rdname tableRow
#' @param table A tabular-like environment to work with.
#' @param row row in the table (1 is top row), including
#' rows of labels.
#' @param withExtras If `TRUE`, include the extras
#' before the line of data, such as `\hline`, etc.
#' @param withData If `TRUE`, include the data.
#' @returns `find_tableRow()` returns a [LaTeX2range] of
#' the entries corresponding to the content of row i of
#' the table.
#' @examples
#' latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
#' parsed <- parseLatex(latex)
#' table <- parsed[[find_tabular(parsed)]]
#' find_tableRow(table, 1)
#'
#' @export
find_tableRow <- function(table, row, withExtras = FALSE, withData = TRUE) {

  drop <- function(skip) {
    if (length(skip)) {
      skip <- unique(skip)
      contentIdx <<- contentIdx[-skip]
      content <<- as_LaTeX2(content[-skip])
    }
  }

  rowidx <- list_idx(table)
  if (!is.null(rowidx)) {
    rows <- table[[rowidx]]
    if (row < 1 || row > length(rows))
      return(integer())
    content <- rows[[row]]
    contentIdx <- seq_along(content)
    if (!withExtras && !withData) {
      if (length(row) > 0 && is_macro(row[[length(row)]], "\\\\"))
        return(LaTeX2range(path = c(rowidx, row)), range = length(row))
      else
        return(NULL)
    }
  } else {
    contentIdx <- find_tableContent(table)
    content <- as_LaTeX2(table[contentIdx])

    # Drop the captions
    idx <- find_captions(content)
    if (length(idx)) {

      # not done yet

      idx <- unlist(attr(idx, "extra"))
      drop(idx)
    }

    breaks <- contentIdx[find_macro(content, "\\\\")]
    if (row <= 0 || row > length(breaks) + 1)
      return(NULL)

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
      return(LaTeX2range(NULL, contentIdx))
  }
  extras <- find_extras(content)

  if (withExtras)   # just extras...
    result <- contentIdx[extras]
  else {            # just data
    drop(extras)
    if (length(contentIdx))
      result <- min(contentIdx):max(contentIdx)
    else
      return(NULL)
  }
  if (is.null(rowidx))
    LaTeX2range(NULL, result)
  else
    LaTeX2range(c(rowidx, row), result)
}

list_idx <- function(table) {
  if (isTRUE(attr(table, "has_itemlist")) &&
      is_itemlist(table[[length(table)]]))
    length(table)
  else
    NULL
}

#' @rdname tableRow
#' @returns `tableRow()` returns a [LaTeX2] object containing
#' all of the table content in the row.
#' @examples
#' tableRow(table, 1)
#' tableRow(table, 1, withExtras = TRUE)
#'
#' @export
tableRow <- function(table, row, withExtras = FALSE, withData = TRUE) {
  if (!is.null(rowidx <- list_idx(table))) {
    row <- table[[c(rowidx, row)]]
    if (withExtras && withData)
      as_LaTeX2(row)
    else if (!withExtras && !withData)
      as_LaTeX2(NULL)
    else {
      if (is.null(colidx <- list_idx(row))) {
          row <- split_row(row)
          colidx <- list_idx(row)
      }
      if (withExtras) # && !withData
        as_LaTeX2(row[seq_len(colidx - 1)])
      else  #  !withExtras && withData
        as_LaTeX2(row[[colidx]])
    }
  } else
    as_LaTeX2(get_range(table, find_tableRow(table, row, withExtras, withData)))
}

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

  rowidx <- list_idx(table)
  if (is.null(rowidx)) {
    table <- split_table(table)
    rowidx <- list_idx(table)
  }
  rows <- table[[rowidx]]
  if (row > length(rows) + 1) {
    # Need to insert rows
    blank <- split_row(blankRow(table))
    for (i in length(rows) + seq_len(row - length(rows) - 1))
      rows[[i]] <- blank
  }
  rows[[row]] <- split_row(value)
  table[[rowidx]] <- rows

  table
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
  eol <- find_macro(row, "\\\\", all = FALSE)
  if (length(eol) > 1)
    warning("This row has a line break.  Only first line used")
  if (!length(eol))
    eol <- length(row) + 1
  br <- c(0, amp[amp < eol], eol)
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

#' Split up a table by rows or columns
#' @param table A tabular-like environment to work with.
#' @returns A [LaTeX2item] object which is the same table
#' with an [ITEMLIST] holding the rows.  The attribute
#' `has_itemlist` will be set to `TRUE`.
#' @export
#' @examples
#' latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
#' parsed <- parseLatex(latex)
#' table <- split_table(parsed[[find_tabular(parsed)]])
#' print(latex2(table), tags = TRUE)

split_table <- function(table) {
  if (!is.null(list_idx(table)))
    return(table)
  start <- min(find_tableRow(table, 1, withExtras = TRUE)$range)
  if (length(start)) {
    idx <- start:length(table)
    contents <- table[idx]
    linebreaks <- find_macro(contents, "\\\\", all = TRUE)
    rows <- split_latex(table[idx], linebreaks, include = TRUE)
    for (i in seq_along(rows))
      rows[[i]] <- split_row(rows[[i]])
    table <- drop_items(table, idx)
    table <- insert_values(table, length(table) + 1, rows)
    structure(table, has_itemlist = TRUE)
  } else
    table
}

#' @rdname split_table
#' @param row A list of items from a single row of a table.
#' @returns A [LaTeX2item] object which is the same row
#' with an [ITEMLIST] holding the cells.  The attribute
#' `has_itemlist` will be set to `TRUE`.
#' @export
#' @examples
#' row <- split_row(tableRow(table, 2))
#' print(latex2(row), tags = TRUE)

split_row <- function(row) {
  colidx <- list_idx(row)
  if (!is.null(colidx))
    return(row)

  extras <- find_extras(row)
  if (length(extras))
    start <- max(extras) + 1
  else
    start <- 1

  if (start <= length(row)) {
    breaks <- find_catcode(row, ALIGN, all = TRUE)
    cols <- split_latex(row[start:length(row)], breaks, include = TRUE)
    cols <- expandMulticolumn(cols)
    row <- drop_items(row, start:length(row))
    row[[start]] <- cols
  }

  structure(new_itemlist(row), has_itemlist = TRUE)
}
