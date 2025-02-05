#' Retrieve source from beyond the end of the document.
#'
#' @param text Character vector holding source.
#' @param items Parsed version of `text`.
#'
#' @note The line numbering in the output matches what a
#' text editor would see; embedded newlines in `text` will
#' result in separate lines in the output.
#' @returns The part of `text` that follows `\end{document}`
#' other than a single newline, named according to the
#'  original line numbers.
#' @export
#' @importFrom utils getSrcref
#' @examples
#' # line:  1                  2                3
#' text <- "\\begin{document}\n\\end{document}\nnotes"
#' get_leftovers(text)
get_leftovers <- function(text, items = parseLatex(text)) {
  text <- readLines(textConnection(text))

  lastitem <- items[[length(items)]]
  srcref <- getSrcref(lastitem)
  if (is.null(srcref))
    stop("Unable to find end of document.")
  lastline <- srcref[3]
  lastchar <- srcref[6]
  text <- text[lastline:length(text)]
  if (length(text)) {
    text[1] <- substring(text[1], lastchar + 1)
    names(text) <- seq_along(text) + lastline - 1
    if (text[1] == "" || text[1] == "\n")
      text <- text[-1]
  }
  text
}
