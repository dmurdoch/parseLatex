% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tablecell.R
\name{tableCell}
\alias{tableCell}
\alias{find_tableCell}
\alias{tableCell<-}
\title{Work with table cells}
\usage{
find_tableCell(table, row, col)

tableCell(table, row, col)

tableCell(table, row, col, asis = FALSE) <- value
}
\arguments{
\item{table}{A tabular-like environment to work with.}

\item{row, col}{row and column in the table.}

\item{asis}{Should blanks be added around the value?}

\item{value}{The content to be inserted into the cell.  This
can be a \link{LaTeX2} object, or a character string that will be
converted to one.}
}
\value{
\code{find_tableCell()} returns a \link{LaTeX2range} object
giving the location of the requested cell.

\code{tableCell()} returns a \link{LaTeX2} object containing
all of the table content in the cell (but not the &).
}
\description{
These functions work with the content of cells in
tabular-like environments.  Cells are numbered with the first
row (typically column titles) being row 1.  Rules (i.e.
horizontal lines) are not considered part of a cell.
}
\details{
\code{find_tableCell()} returns \code{NULL} if the requested
cell is missing because an earlier cell covered multiple
columns.  It signals an error if a request is made beyond
the bounds of the table.

Unless \code{asis = TRUE}, \code{tableContent(table) <- value}
will add blanks
at the start end end if not present, to make the result
more readable.

If \code{col} is higher than the current table width,
the assignment will fail with an error.  If only \code{row}
is too high, blank lines will be added and it should
succeed.
}
\examples{
latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex")
parsed <- parseLatex(latex)
table <- prepare_table(parsed[[find_tabular(parsed)]], do_cells = TRUE)
find_tableCell(table, 1, 2)

tableCell(table, 1, 2)

tableCell(table, 5, 2) <- " d "
table

}
