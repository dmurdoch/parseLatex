# Magic mirror for latex tables --------------
#' @export
magic_mirror_latex <- function(kable_input){
  table_info <- list(tabular = NULL, booktabs = FALSE, align = NULL,
                     valign = NULL, ncol = NULL, nrow = NULL, colnames = NULL,
                     rownames = NULL, caption = NULL, caption.short = NULL,
                     contents = NULL,
                     centering = FALSE, table_env = FALSE)

  parsed <- parseLatex(kable_input)

  # Outer wrapper
  outer <- Filter(function(item) is_env(item, "table"), parsed)
  if (!length(outer))
    inner <- parsed
  else {
    if (length(outer) > 1) {
      warning(length(outer), " tables found.  Using the first.")
      outer <- outer[1]
    }
    inner <- outer[[2]]
  }

  # Tabular
  table <- Filter(function(item) !is.null(tabular_type(item)), inner)
  if (!length(table))
    stop("No tabular environment found.")
  if (length(table) > 1)
    warning(length(table), " tabular environments found; using the first")
  table <- table[[1]]
  table_info$tabular <- tabular_type(table)

  contents <- table[[2]]
  # Booktabs
  table_info$booktabs <- length(Filter(function(item) is_macro(item, "\\toprule"), contents)) > 0

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
  if (str_detect(kable_input, "caption\\[")) {
    caption_line <- str_match(kable_input, "\\\\caption(.*)\\n")[2]
    table_info$caption.short <- str_match(caption_line, "\\[(.*?)\\]")[2]
    table_info$caption <- substr(caption_line,
                                 nchar(table_info$caption.short) + 4,
                                 nchar(caption_line))
  } else {
    table_info$caption <- str_match(kable_input, "caption\\{(.*?)\\n")[2]
  }
  if (table_info$tabular == "longtable") {
    table_info$caption <- str_sub(table_info$caption, 1, -4)
  } else {
    table_info$caption <- str_sub(table_info$caption, 1, -2)
  }
  # Contents
  table_info$contents <- str_match_all(kable_input, "\n(.*)\\\\\\\\")[[1]][,2]
  table_info$contents <- regex_escape(table_info$contents, T)
  if (table_info$tabular == "longtable" & !is.na(table_info$caption) &
      !str_detect(kable_input, "\\\\begin\\{table\\}\\n\\n\\\\caption")) {
    table_info$contents <- table_info$contents[-1]
  }
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
  if (table_info$booktabs & !grepl("\\\\midrule", kable_input)) {
    table_info$colnames <- NULL
    table_info$position_offset <- 0
  } else {
    table_info$colnames <- str_split(table_info$contents[1], " \\& ")[[1]]
    table_info$position_offset <- 1
  }
  # Row names
  table_info$rownames <- str_extract(table_info$contents, "^[^ &]*")

  table_info$centering <- grepl("\\\\centering", kable_input)

  table_info$table_env <- (!is.na(table_info$caption) &
                             table_info$tabular != "longtable") ||
    grepl("\\\\begin\\{table\\}", kable_input)

  return(table_info)
}
