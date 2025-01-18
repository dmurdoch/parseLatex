
#' Parse LaTeX code
#'
#' The \code{parseLatex} function parses LaTeX source, producing a structured object.
#' @param text A character vector containing LaTeX source code.
#' @param filename A filename to use in syntax error messages.
#' @param verbose If \code{TRUE}, print debug error messages.
#' @param verbatim A character vector containing the names of \LaTeX environments holding verbatim text.
#' @param verb A character vector containing LaTeX macros that should be assumed to hold verbatim text.
#'
#' @returns `NULL`, so far.
#' @export
#' @importFrom utils getParseData
#' @importFrom tools deparseLatex
#'
#' @examples
#' parsed <- parseLatex(r"(fran\c{c}ais)")
#' parsed
#' getParseData(parsed)
parseLatex <- function(text,
                       filename = deparse1(substitute(text)),
                       verbose = FALSE,
                       verbatim = c("verbatim", "verbatim*",
                        "Sinput", "Soutput"),
                       verb = "\\Sexpr") {

}
