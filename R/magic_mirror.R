# Magic mirror for latex tables --------------
#' @export
magic_mirror_latex <- function(kable_input){

  table_info <- list(tabular = NULL, booktabs = FALSE, align = NULL,
                     valign = NULL, ncol = NULL, nrow = NULL, colnames = NULL,
                     rownames = NULL, caption = NULL, caption.short = NULL,
                     contents = NULL,
                     centering = FALSE, table_env = FALSE)

  # parsed - the whole input.
  # outeridx - the index of the table environment or NULL if none
  # inner - the contents of the table env, or parsed if none
  # contents - everything inside the tabular environment (including
  #            the options)

  parsed <- parseLatex(kable_input)

  # Outer wrapper
  outeridx <- find_env(parsed, "table")
  if (!length(outeridx))
    inner <- parsed
  else {
    if (length(outeridx) > 1) {
      warning(length(outeridx), " tables found.  Using the first.")
      outeridx <- outeridx[1]
    }
    inner <- as_LaTeX2(parsed[[c(outeridx, 2)]])
  }

  # Tabular
  tableidx <- find_env(inner, c("tabular", "longtable"))
  if (!length(tableidx))
    stop("No tabular environment found.")
  if (length(tableidx) > 1) {
    warning(length(tableidx), " tabular environments found; using the first")
    tableidx <- tableidx[1]
  }
  table <- inner[[tableidx]]
  table_info$tabular <- envName(table)

  contents <- table[[2]]
  # Booktabs
  table_info$booktabs <- length(find_macro(contents, "\\toprule")) > 0

  # Align
  options <- deparseLatex(brace_options(contents))
  table_info$align <- gsub("\\|", "", options)
  table_info$align_vector <- unlist(strsplit(table_info$align, ""))
  table_info$align_vector_origin <- table_info$align_vector
  # valign
  valign <- bracket_options(contents)
  if (is.null(valign))
    valign <- "t"
  table_info$valign3 <- valign
  table_info$valign2 <- paste0("\\[", valign, "\\]")
  table_info$valign <- paste0("[", valign, "]")

  table_info$begin_tabular <- paste0("\\\\begin\\{", table_info$tabular, "\\}",
                                     table_info$valign2)
  table_info$end_tabular <- paste0("\\\\end\\{", table_info$tabular, "\\}")
  # N of columns
  table_info$ncol <- nchar(table_info$align)

  # Caption
  if (table_info$tabular == "longtable")
    items <- contents
  else
    items <- inner

  captionidx <- find_macro(items, c("\\caption", "\\caption*"))

  if (length(captionidx)) {
    cap <- bracket_options(items, drop = FALSE, start = captionidx[1] + 1)
    if (length(cap))
      table_info$caption.short <- deparseLatex(cap)
    cap <- brace_options(items, drop = FALSE, start = captionidx + 1)
    if (!is.null(cap))
      table_info$caption <- deparseLatex(cap)
  }

  # Contents
  items <- contents
  # Drop the tabular options
  skip <- length(bracket_options(items, drop = FALSE)) +
          length(brace_options(items, drop = FALSE))
  if (skip > 0)
    items <- drop_items(items, 1:skip)

  # Drop the captions
  skip <- find_macro(items, c("\\caption", "\\caption*"))
  items <- drop_items(items, c(skip, skip+1))

  # Drop the rules
  skip <- find_macro(items, c("\\hline", "\\toprule", "\\midrule", "\\bottomrule"))
  items <- drop_items(items, skip)

  # Drop all newlines
  skip <- find_catcode(items, 5)
  items <- drop_items(items, skip)

  # Extract the rows
  breaks <- find_macro(items, "\\\\")
  rows <- split_items(items, breaks)
  table_info$contents <- sapply(rows, deparseLatex)

  if (!is.null(attr(kable_input, "n_head"))) {
    n_head <- attr(kable_input, "n_head")
    table_info$new_header_row <- table_info$contents[seq(n_head - 1, 1)]
    table_info$contents <- table_info$contents[-seq(1, n_head - 1)]
    table_info$header_df <- extra_header_to_header_df(table_info$new_header_row)
    table_info$new_header_row <- paste0(table_info$new_header_row, "\\\\\\\\")
  }
  table_info$nrow <- length(table_info$contents)
  table_info$duplicated_rows <- (sum(duplicated(table_info$contents)) != 0)
  # Column names
  if (table_info$booktabs & length(find_macro(contents, "\\midrule")) == 0) {
    table_info$colnames <- NULL
    table_info$position_offset <- 0
  } else {
    row <- select_items(items, seq_len(breaks[1] - 1))
    align <- c(find_catcode(row, 4), length(row) + 1)
    table_info$colnames <- sapply(split_items(row, align),
                                  deparseLatex)
    table_info$position_offset <- 1
  }
  # Row names
  table_info$rownames <- str_extract(table_info$contents, "^[^ &]*")

  table_info$centering <- length(find_macro(inner, "\\centering")) > 0

  table_info$table_env <- !is.null(outeridx)

  return(table_info)
}
