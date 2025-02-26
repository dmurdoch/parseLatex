#' Show errors in parsed Latex object
#'
#' @param x A [LaTeX2] object.
#' @param repeatSrcline Repeat the source line when it has multiple errors?
#' @param errorMsgTwice Show the error message at both the start
#' and end of a multiline error?
#' @param lineNumbers Show line numbers on output?
#' @param showAllLines Show all lines whether they have errors or
#' not?
#'
#' @returns A list of paths to errors, invisibly.
#' @export
#'
#' @examples
#' parsed <- parseLatex("\\end{baz} \\begin{foo} \n \\begin{bar}  $1+1\n4",
#'                      recover = TRUE, showErrors = FALSE)
#' showErrors(parsed)
showErrors <- function(x, repeatSrcline = FALSE, errorMsgTwice = FALSE, lineNumbers = TRUE, showAllLines = FALSE) {
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
    if (lineNumbers) {
      linenums <- paste0(format(seq_along(text)), ": ")
      indent <- paste(rep(" ", nchar(linenums[1]), collapse=""))
    } else {
      linenums <- rep("", length(text))
      indent <- ""
    }
    if (showAllLines)
      showlines <- seq_along(text)
    else
      showlines <- sort(unique(c(start_line, end_line)))
    for (line in showlines) {
      if (showAllLines || !repeatSrcline) {
        cat(linenums[line], text[line], "\n", sep = "")
      }
      firsterror <- TRUE
      # Errors ending on this line
      i1 <- which(start_line < line & end_line == line)
      i1 <- i1[order(-start_line[i1], -start_col[i1])]
      for (i in i1) {
        if (repeatSrcline && !(showAllLines && firsterror)) {
          firsterror <- FALSE
          cat(linenums[line], text[line], "\n", sep = "")
        }
        marker <- c(indent,
                    rep("-", end_col[i] - 1),
                    ">")
        cat(marker, "\n", sep = "")
        err <- x[[errs[[i]]]]
        cat(indent, attr(err, "errormsg"), "\n", sep = "")
      }
      # Errors all on one line
      i1 <- which(start_line == line & end_line == line)
      i1 <- i1[order(start_col[i1])]
      for (i in i1) {
        if (repeatSrcline && !(showAllLines && firsterror)) {
          firsterror <- FALSE
          cat(linenums[line], text[line], "\n", sep = "")
        }
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
      i1 <- which(start_line == line & end_line > line)
      i1 <- i1[order(start_col[i1])]
      for (i in i1) {
        if (repeatSrcline && !(showAllLines && firsterror)) {
          firsterror <- FALSE
          cat(linenums[line], text[line], "\n", sep = "")
        }
        marker <- c(indent,
                    rep(" ", start_col[i] - 1), "<",
                    rep("-", nchar(text[line]) - start_col[i]))
        cat(marker, "\n", sep = "")
        if (errorMsgTwice) {
          err <- x[[errs[[i]]]]
          cat(indent, attr(err, "errormsg"), "\n", sep = "")
        }
      }
    }
  }
  cat("\n")
  invisible(errs)
}
