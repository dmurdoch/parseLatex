
#' Parse LaTeX code
#'
#' The \code{parseLatex} function parses LaTeX source, producing a structured object.
#' @aliases LaTeX2 LaTeX2item
#' @name parseLatex_fn
#' @param text A character vector containing LaTeX source code.
#' @param verbose If \code{TRUE}, print debug error messages.
#' @param verbatim A character vector containing the names of \LaTeX environments holding verbatim text.
#' @param verb A character vector containing LaTeX macros that should be assumed to hold verbatim text.
#' @param catcodes A list or dataframe holding LaTeX "catcodes",
#' such as [defaultCatcodes].
#'
#' @details
#' Some versions of LaTeX such as `pdflatex` only handle ASCII
#' inputs, while others such as `xelatex` allow Unicode input.
#' `parseLatex` allows Unicode input.
#'
#' During processing of LaTeX input, an interpreter can change
#' the handling of characters as it goes, using the `\catcode` macro
#' or others such as `\makeatletter`.  However, `parseLatex()` is purely
#' a parser, not an interpreter, so it can't do that, but
#' the user can change handling for the whole call using the
#' `catcodes` argument.
#'
#' `catcodes` should be a list or dataframe
#' with at least two columns:
#'
#'  - `char` should be a column of single characters.
#'  - `catcode` should be a column of integers in the range 0 to 15
#'  giving the corresponding catcode.
#'
#'  During parsing, `parseLatex` will check these values first.
#'  If the input character doesn't match anything, then it will
#'  be categorized:
#'
#'  - as a letter (catcode 11) using the ICU function
#'  `u_hasBinaryProperty(c, UCHAR_ALPHABETIC)` (or `iswalpha(c)` on
#'  Windows),
#'  - as a control
#'  character (catcode 15) if its code point is less than 32,
#'  - as "other" (catcode 12) otherwise.
#'
#'
#' @returns `parseLatex` returns parsed Latex in a list with class `"LaTeX2"`.  Items in the list have class `"LaTeX2item"`.
#' @seealso LaTeX2, LaTeX2item
#' @export
#' @importFrom utils getParseData
#' @importFrom tools deparseLatex
#'
#' @examples
#' parsed <- parseLatex(r"(fran\c{c}ais)")
#' parsed
parseLatex <- function(text,
                       verbose = FALSE,
                       verbatim = c("verbatim", "verbatim*",
                        "Sinput", "Soutput"),
                       verb = "\\Sexpr",
                       catcodes = defaultCatcodes) {

  text <- paste(text, collapse="\n")
  stopifnot(all(nchar(catcodes$char, "chars") == 1))
  codepoint <- utf8ToInt(paste0(catcodes$char, collapse = ""))
  catcode <- as.integer(catcodes$catcode)
  if (!(all(catcode %in% 0:15)))
    stop("catcodes must be in the range 0 to 15")
  if (length(codepoint) != length(catcode))
    stop("catcodes must have one char per catcode")
  .External(C_parseLatex, text, as.logical(verbose),
            as.character(verbatim), as.character(verb),
            codepoint, catcode)

}

#' The default "catcodes" used by [parseLatex].
#' @name defaultCatcodes
#' @details `defaultCatcodes` is a dataframe containing the
#' default catcode definitions.
#' @examples
#' # \makeatletter has no effect by default...
#' unclass(parseLatex("\\makeatletter\\internal@macro"))
#' # ... but the effect can be simulated
#' atletter <- rbind(defaultCatcodes,
#'                   data.frame(char="@", catcode=11))
#' unclass(parseLatex("\\makeatletter\\internal@macro",
#'                    catcodes = atletter))
#'
#' @export
defaultCatcodes <-
  data.frame(char = c("\\", "{", "}", "$", "&", "\n", "\r", "#", "^", "_", " ", "\t", "%"),
          catcode = c(0,     1,   2,   3,   4,    5,    5,   6,   7,  8,  10,   10,   14 ))

#' @rdname parseLatex_fn
#' @export
print.LaTeX2item <- function(x, ...) {
  cat(paste0(latexTag(x), ": ", deparseLatex(list(x)), "\n"))
}

#' @rdname parseLatex_fn
#' @param x Object to work on.
#' @param tags Whether to display LaTeX2 tags.
#' @param ... Extra parameters to pass to `deparseLatex`.
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
        cat(envName(item), ":\n")
        lapply(item, showItem, indent + 2)
      } else if (tag == "BLOCK") {
        cat("{\n")
        lapply(item, showItem, indent + 2)
        cat(rep(" ", indent), collapse="")
        cat("}\n")
      } else if (tag == "SPECIAL") {
        code <- attr(item, "catcode")
        if (code == NEWLINE)
          cat("NEWLINE\n")
        else
          cat(catcodes[1 + attr(item, "catcode")], ":", item, "\n")
      } else if (tag == "DEFINITION") {
        lapply(item, showItem, indent + 2)
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
#' Convert latex object into character vector
#'
#' @param x A latex object.
#' @param dropBraces Whether to drop unnecessary braces.
#'
#' @returns `deparseLatex` returns character vector corresponding
#' to the parsed Latex.
#' @export
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
                       DEFINITION = Recall(a),
                       ENVIRONMENT = c(
                         "\\begin{", envName(a), "}",
                         Recall(a),
                         "\\end{", envName(a), "}"),
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

#' The parseLatex package
#' @name parseLatex_pkg
#' @useDynLib parseLatex, .registration=TRUE
"_PACKAGE"
