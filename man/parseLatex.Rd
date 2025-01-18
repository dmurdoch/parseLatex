\name{parseLatex}
\alias{parseLatex}
\title{
Parse LaTeX Code
}
\description{
The \code{parseLatex} function parses LaTeX source, producing a structured object.
}
\usage{
parseLatex(text, filename = deparse1(substitute(text)), verbose = FALSE, verbatim = c("verbatim", "verbatim*", "Sinput", "Soutput"), verb = "\\Sexpr")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text}{
A character vector containing LaTeX source code.
}
  \item{filename}{
A filename to use in syntax error messages.
}
  \item{verbose}{
If \code{TRUE}, print debug error messages.
}
  \item{verbatim}{
A character vector containing the names of \LaTeX environments holding verbatim text.
}
  \item{verb}{
A character vector containing LaTeX macros that should be assumed to hold verbatim text.
}
}
\value{
\code{NULL}, so far.
}
\author{
Duncan Murdoch
}

\seealso{
The \link[tools]{parseLatex} function is the basis for this function.
}
\examples{
parseLatex("fran\\\\c{c}ais")
}
