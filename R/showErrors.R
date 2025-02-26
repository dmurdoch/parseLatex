#' Show errors in parsed Latex object
#'
#' @param x A [LaTeX2] object.
#'
#' @returns A list of paths to errors, invisibly.
#' @export
#'
#' @examples
#' parsed <- parseLatex("\\begin{foo} abc \\end{bar} def",
#'                      recover = TRUE)
#' showErrors(parsed)
showErrors <- function(x) {
  errs <- path_to(x, is_error, all = TRUE)
  if (length(errs) == 0)
    cat("No errors.\n")
  else {
    start_line <- end_line <- start_col <- end_col <- integer(length(errs))
    for (i in seq_along(errs)) {
      err <- x[[errs[[i]]]]
      srcref <- getSrcref(err)
      start_line[i] <- srcref[1]
      end_line[i] <- srcref[3]
      start_col[i] <- srcref[5]
      end_col[i] <- srcref[6]
    }
    text <- strsplit(paste(deparseLatex(x), collapse = "\n"),
                     "\n")[[1]]
    linenums <- paste0(format(seq_along(text)), ": ")
    indent <- rep(" ", nchar(linenums[1]))
    for (line in sort(unique(c(start_line, end_line)))) {
      # Errors ending on this line
      for (i in which(start_line < line & end_line == line)) {
        cat(linenums[line], text[line], "\n", sep = "")
        marker <- c(indent,
                    rep("-", end_col[i] - 1),
                    ">")
        cat(marker, "\n", sep = "")
        err <- x[[errs[[i]]]]
        cat(indent, attr(err, "errormsg"), "\n", sep = "")
      }
      # Errors all on one line
      for (i in which(start_line == line & end_line == line)) {
        cat(linenums[line], text[line], "\n", sep = "")
        marker <- c(indent, rep(" ", start_col[i] - 1))
        if (start_col[i] == end_col[i])
          marker <- c(marker, "^")
        else
          marker <- c(marker, "<",
                      rep("-", end_col[i] - start_col[i] - 1),
                      ">")
        cat(marker, "\n", sep = "")
        err <- x[[errs[[i]]]]
        cat(indent, attr(err, "errormsg"), "\n", sep = "")
      }
      # Errors starting on this line
      for (i in which(start_line == line & end_line > line)) {
        cat(linenums[line], text[line], "\n", sep = "")
        marker <- c(indent,
                    rep(" ", start_col[i] - 1), "<",
                    rep("-", nchar(text[line]) - start_col[i]))
        cat(marker, "\n", sep = "")
        err <- x[[errs[[i]]]]
        cat(indent, attr(err, "errormsg"), "\n", sep = "")
      }
    }
  }
  invisible(errs)
}
