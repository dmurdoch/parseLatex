% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{Utilities}
\alias{Utilities}
\alias{drop_items}
\alias{select_items}
\alias{drop_whitespace}
\alias{trim_whitespace}
\alias{include_whitespace}
\alias{split_list}
\alias{split_latex}
\alias{new_block}
\alias{new_env}
\title{Miscellaneous utilities}
\usage{
drop_items(items, which)

select_items(items, which)

drop_whitespace(items)

trim_whitespace(items)

include_whitespace(items, which)

split_list(items, splits)

split_latex(items, splits)

new_block(...)

new_env(name, ...)
}
\arguments{
\item{items}{A \link{LaTeX2} object or list of items, or
a \link{LaTeX2item} which is a list.}

\item{which}{Which items to operate on.}

\item{splits}{Which items divide the parts?}

\item{...}{Items to be passed to \code{latex2()}.}

\item{name}{The desired environment name.}
}
\value{
\code{drop_items()} returns the list of items with specific items removed.

\code{select_items()} returns the list of subsetted items.

\code{drop_whitespace()} returns the items with
whitespace (blanks, tabs, newlines) removed.

\code{trim_whitespace()} returns the items with
leading and trailing whitespace (blanks, tabs, newlines) removed.

\code{include_whitespace()} returns \code{which} with
following whitespace (blanks, tabs, newlines) included.

\code{split_list()} returns a list of pieces
separated at the splits.

\code{split_latex()} returns a list of pieces
separated at the splits.  Each piece is marked as
a \link{LaTeX2} object.

\code{new_block()} returns a \code{BLOCK} item containing the items.

\code{new_env()} returns an environment item containing the other items.
}
\description{
Miscellaneous utilities
}
\note{
\code{drop_whitespace()} will drop the whitespace that separates text items, so deparsing will merge
them into a single item.
}
\examples{
new_block(parseLatex("abc"))
new_env("itemize", parseLatex("\\\\item An item"))
}
\seealso{
\code{drop_whitespace()} does not act recursively; use \link{reduce_whitespace} for that.
}
