# These are the TeX catcodes

#' @export
ESCAPE      <- 0   # backslash
#' @export
LBRACE      <- 1   # {
#' @export
RBRACE      <- 2   # }
#' @export
MATH        <- 3   # $
#' @export
ALIGN       <- 4   # &
#' @export
NEWLINE     <- 5   # \n or \r
#' @export
PARAM       <- 6   # the # char
#' @export
SUPER       <- 7   # ^
#' @export
SUB         <- 8   # _
#' @export
IGNORE      <- 9   # nul
#' @export
SPACE       <- 10  # blank or tab
#' @export
LETTER      <- 11  # letters
#' @export
OTHER       <- 12  # everything else
#' @export
ACTIVE      <- 13  # "active" characters, only ~
#' @export
COMMENT     <- 14  # %
#' @export
INVALID     <- 15  # \x7F
