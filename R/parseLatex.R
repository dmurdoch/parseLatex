
#' Parse LaTeX code
#'
#' The \code{parseLatex} function parses LaTeX source, producing a structured object.
#' @param text A character vector containing LaTeX source code.
#' @param filename A filename to use in syntax error messages.
#' @param verbose If \code{TRUE}, print debug error messages.
#' @param verbatim A character vector containing the names of \LaTeX environments holding verbatim text.
#' @param verb A character vector containing LaTeX macros that should be assumed to hold verbatim text.
#'
#' @returns Parsed Latex in list with class `"LaTeX2"`.
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

  ## the internal function must get some sort of srcfile
  srcfile <- srcfilecopy(filename, text, file.mtime(filename))
  text <- paste(text, collapse="\n")
  .External(C_parseLatex, text, srcfile, as.logical(verbose), as.character(verbatim), as.character(verb))

}

#' @rdname parseLatex
#' @export
print.LaTeX2 <- function(x, tags = FALSE, ...) {
  if (tags) {
    showItem <- function(item, indent = 0) {
      catcodes <- c(
        "ESCAPE", "LBRACE", "RBRACE", "MATH",
        "ALIGN",  "NEWLINE","PARAM",  "SUPER",
        "SUB",    "IGNORE", "SPACE",  "LETTER",
        "OTHER",  "ACTIVE", "COMMENT","INVALID"
      )
      cat(rep(" ", indent), collapse="")
      tag <- attr(item, "latex_tag")
      if (tag  == "ENVIRONMENT") {
        cat(item[[1]], ":\n")
        lapply(item[[2]], showItem, indent + 2)
      } else if (tag == "BLOCK") {
        cat("{\n")
        lapply(item, showItem, indent + 2)
        cat(rep(" ", indent), collapse="")
        cat("}\n")
      } else if (tag == "SPECIAL") {
        code <- attr(item, "catcode")
        if (code == 5)
          cat("NEWLINE\n")
        else
          cat(catcodes[1 + attr(item, "catcode")], ":", item, "\n")
      } else
        cat(tag, ":", item, "\n")
    }
    lapply(x, showItem)
  } else {
    cat(deparseLatex(x, ...), "\n")
  }
  invisible(x)
}

# This converts a latex object into a single element character vector
deparseLatex <- function(x, dropBraces = FALSE)
{
  specials <- c("\\", "#", "$", "%", "&", "~", "_", "^", "{", "}")
  result <- character()
  lastTag <- "TEXT"
  expectArg <- FALSE
  for (i in seq_along(x)) {
    a <- x[[i]]
    tag <- attr(a, "latex_tag")
    if (is.null(tag)) tag <- "NULL"
    result <- c(result,
                switch(tag,
                       VERB = ,
                       MACRO = ,
                       COMMENT = a,
                       SPECIAL = ,
                       TEXT = c(if (lastTag == "MACRO" && expectArg && grepl("^[[:alpha:]]", a))
                         ## restore space that the parser has eaten ('\item text')
                         " ",
                         a),
                       BLOCK = if (dropBraces && !expectArg)
                         Recall(a)
                       else
                         c("{", Recall(a), "}"),
                       ENVIRONMENT = c(
                         "\\begin{", a[[1L]], "}",
                         Recall(a[[2L]]),
                         "\\end{", a[[1L]], "}"),
                       MATH = c("$", Recall(a), "$"), # \( and \) parse as MACRO
                       DISPLAYMATH = c("$$", Recall(a), "$$"),
                       NULL = stop("Internal error, no tag", domain = NA)
                ))
    lastTag <- tag
    expectArg <-
      if (tag == "MACRO")
        !(a %in% paste0("\\", c(specials, "(", ")")))
    else
      expectArg &&
      tag %in% c("BLOCK", "COMMENT") # \cmd{}{}, \cmd%
    ## currently ignoring \cmd  {}, \cmd[]{}, \cmd*{}
  }
  paste(result, collapse="")
}


#' @name parseLatex
#' @useDynLib parseLatex, .registration=TRUE
"_PACKAGE"
