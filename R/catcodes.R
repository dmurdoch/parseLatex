# These are the TeX catcodes

ESCAPE      <- 0   # backslash
LBRACE      <- 1   # {
RBRACE      <- 2   # }
MATH        <- 3   # $
ALIGN       <- 4   # &
NEWLINE     <- 5   # \n or \r
PARAM       <- 6   # the # char
SUPER       <- 7   # ^
SUB         <- 8   # _
IGNORE      <- 9   # nul
SPACE       <- 10  # blank or tab
LETTER      <- 11  # letters
OTHER       <- 12  # everything else
ACTIVE      <- 13  # "active" characters, only ~
COMMENT     <- 14  # %
INVALID     <- 15  # \x7F
