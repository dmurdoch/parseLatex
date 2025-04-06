find_extras <- function(row) {
  # prepare_row puts the extras in item 1.
  if (has_itemlist(row))
    return(1L)

  # First, find an upper limit
  upper <- find_catcode(row, ALIGN, all = FALSE)
  if (!length(upper))
    upper <- find_macro(row, "\\\\", all = FALSE)
  if (!length(upper))
    upper <- length(row) + 1

  if (upper < 2)
    return(NULL)

  row <- row[1:(upper - 1)]

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

  has_itemlist <- has_itemlist(table)

  if (has_itemlist) {
    if (row < 1 || row > length(table) - 2)
      return(NULL)
    content <- table[[row + 1]]
    contentIdx <- seq_along(content)
    if (!withExtras && !withData) {
      if (length(content) > 0 && is_macro(content[[length(content)]], "\\\\"))
        return(LaTeX2range(path = c(row + 1)), range = length(row))
      else
        return(NULL)
    }
  } else {
    contentIdx <- find_tableContent(table)
    content <- as_LaTeX2(table[contentIdx])

    # Drop the captions
    idx <- find_caption(content)
    if (length(idx)) {
      idx <- attr(idx, "extra")
      drop(seq_len(max(idx$range)))
    }

    breaks <- contentIdx[find_macro(content, "\\\\")]
    if (row <= 0 || row > length(breaks) + 1)
      return(NULL)

    if (!withExtras && !withData) {
      if (row > length(breaks))
        return(NULL)
      else
        return(breaks[row])
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
  if (!has_itemlist)
    LaTeX2range(NULL, result)
  else
    LaTeX2range(row + 1, result)
}

list_idx <- function(table) {
  attr(table, "itemlist_start")
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
  if (has_itemlist(table)) {
    row <- table[[row + 1]]
    if (withExtras && withData)
      row
    else if (!withExtras && !withData)
      as_LaTeX2(NULL)
    else {
      if (!has_itemlist(row))
          row <- prepare_row(row)
      if (withExtras) # && !withData
        result <- as_LaTeX2(row[1])
      else  #  !withExtras && withData
        result <- as_LaTeX2(row[2:length(row)])
      structure(result, has_itemlist = TRUE)
    }
  } else
    as_LaTeX2(get_range(table, find_tableRow(table, row, withExtras, withData)))
}

blankRow <- function(table) {
  latex2(paste0(rep(" & ", tableNcol(table) - 1), collapse = ""), "\\\\")
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

  has_itemlist <- has_itemlist(table)
  if (!has_itemlist)
    table <- prepare_table(table)

  value <- flatten_itemlists(as_LaTeX2(value))
  if (!asis) {
    if (row <= length(table) - 2) {
      if (!withExtras)
        value <- latex2(tableRow(table, row, withExtras = TRUE, withData = FALSE), value)
      if (!withData)
        value <- latex2(value, tableRow(table, row, withExtras = FALSE, withData = TRUE))
    }
    breaks <- find_macro(value, "\\\\")
    if (!length(breaks))
      value <- latex2(value, "\\\\")
    newlines <- find_catcode(value, NEWLINE)
    if (!(1 %in% newlines))
      value <- latex2("\n", value)
  }
  newrows <- length(find_macro(value, "\\\\"))
  n <- tableNrow(table)
  if (row > n) {
    # Need to insert rows
    lastlist <- table[[length(table)]]
    blank <- prepare_row(new_itemlist(latex2("\n", blankRow(table))))
    for (i in (n+1):(row))
      table <- insert_values(table, i, blank, after = TRUE)
  }
  if (newrows > 1) {
    # adding more than one row, so need to re-prepare
    table[[row + 1]] <- new_itemlist(value)
    table <- flatten_itemlists(table)
    table <- prepare_table(table)
  } else  # for just one row, only prepare the row.
    table[[row + 1]] <- prepare_row(value)

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
  # We don't know if the row coming in has already had
  # extras removed, so prepare it again.
  row <- as_LaTeX2(row)
  row <- flatten_itemlists(row)
  row <- prepare_row(row)
  row <- row[-1] # leave off extras
  for (i in seq_along(row)) {
    last <- row[[i]]
    last <- last[[length(last)]]
    if (is_macro(last, "\\\\") || is_catcode(last, ALIGN))
      length(row[[i]]) <- length(row[[i]]) - 1L
  }
  if (deparse)
    result <- vapply(row, deparseLatex, "")
  else
    result <- unclass(row)
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

#' Split up a table by rows
#' @param table A tabular-like environment to work with.
#' @param do_cells Should the rows be prepared too?
#' @returns A [LaTeX2item] object which is the same table
#' but with the contents divided into [ITEMLIST]s.  The first element is an [ITEMLIST]
#' holding everything before the first row, then
#' each row is in its own [ITEMLIST], and finally
#' one more holding everything after the last row. The attribute
#' `has_itemlists` will be set to `TRUE`.
#' @export
#' @examples
#' latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
#' parsed <- parseLatex(latex)
#' table <- prepare_table(parsed[[find_tabular(parsed)]])
#' print(latex2(table), tags = TRUE)

prepare_table <- function(table, do_cells = TRUE) {
  has_itemlist <- has_itemlist(table)
  if (has_itemlist)
    return(table)
  start <- find_tableRow(table, 1, withExtras = TRUE)$range
  if (length(start)) {
    start <- min(start)
    if (start > 1)
      firstlist <- new_itemlist(table[1:(start - 1)])
    else
      firstlist <- new_itemlist(placeholder())
    idx <- start:length(table)
    contents <- table[idx]
    linebreaks <- find_macro(contents, "\\\\", all = TRUE)
    rows <- split_latex(table[idx], linebreaks, include = TRUE)
    if (do_cells)
      for (i in seq_along(rows))
        rows[[i]] <- prepare_row(rows[[i]])
    table <- drop_items(table, 1:length(table))
    table <- insert_values(table, 1, firstlist)
    table <- insert_values(table, 2, rows)
    structure(table, has_itemlist = TRUE)
  } else
    table
}

#' @rdname prepare_table
#' @param row A list of items from a single row of a table.
#' @returns `prepare_row()` returns a [LaTeX2item] object which is the same row
#' with [ITEMLIST]s holding the cells.  The attribute
#' `has_itemlist` will be set to `TRUE`.  The first
#' list will be the "extras" at the start of the row;
#' each cell will be in the following [ITEMLIST]s.
#' The following cell delimiter will be included in the cell.
#' @export
#' @examples
#' row <- prepare_row(tableRow(table, 2))
#' print(latex2(row), tags = TRUE)

prepare_row <- function(row) {
  if (has_itemlist(row))
    return(row)

  row <- flatten_itemlists(row)

  extras <- find_extras(row)
  if (length(extras))
    start <- max(extras) + 1
  else
    start <- 1

  if (start > 1)
    firstlist <- new_itemlist(row[1:(start-1)])
  else
    firstlist <- new_itemlist(placeholder())

  if (start <= length(row)) {
    breaks <- find_catcode(row[start:length(row)], ALIGN, all = TRUE)
    cols <- split_latex(row[start:length(row)], breaks, include = TRUE)
    cols <- expandMulticolumn(cols)
  } else
    cols <- NULL

  structure(new_itemlist(firstlist, cols), has_itemlist = TRUE)
}
