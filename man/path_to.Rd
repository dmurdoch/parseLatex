% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/finders.R
\name{path_to}
\alias{path_to}
\alias{get_item}
\alias{get_items}
\alias{set_item}
\alias{insert_values}
\alias{get_container}
\alias{get_which}
\title{Find path to a particular kind of item}
\usage{
path_to(items, test, ..., all = FALSE)

get_item(items, path = index_to_path(index, items), index)

get_items(items, paths = lapply(indices, index_to_path, items), indices)

set_item(items, path, value)

insert_values(items, path, values, after = FALSE)

get_container(items, path)

get_which(path)
}
\arguments{
\item{items}{A list of latex items.}

\item{test}{Which test function to use.}

\item{...}{Additional parameters to pass to \code{is_fn}.}

\item{all}{Return all paths, or just the first?}

\item{path}{Integer vector of subitems}

\item{index}{Index into the flattened version of \code{items}.}

\item{paths}{List of paths}

\item{indices}{Vector of indices into the flattened version of \code{items}.}

\item{value}{A \link{LaTeX2item} to set as a value.}

\item{values}{A \link{LaTeX2} list or a \link{LaTeX2item}.}

\item{after}{If \code{TRUE}, insert the values after \code{path}.}
}
\value{
\code{path_to()} returns the recursive path to the
first example matching the \code{is_fn} conditions,
or a list of paths to all matching items.

\code{get_item()} returns the item at the given path.  If \code{index} is specified, \code{get_item()} will return that
item in the flattened version of \code{items}.

\code{get_items()} returns the items at the given paths as a \link{LaTeX2} object.  If \code{index} is specified, \code{get_items()} will return those
items in the flattened version of \code{items}.

\code{set_item()} replaces the item at the given path, and returns the modified version of \code{items}.

\code{insert_values()} inserts the \code{values} before
the item mentioned in \code{path} (or after if requested),
and returns the modified version of \code{items}.

\code{get_container()} returns the item
containing the given path

\code{get_which()} returns the index
of the item within its container.
}
\description{
Find path to a particular kind of item
}
\details{
\code{path_to()} does a recursive search in the order
items appear in the deparse.
}
\examples{
latex <- kableExtra::kbl(mtcars[1:2, 1:2], format = "latex", caption = "Sample table")
parsed <- parseLatex(latex)
parsed
path <- path_to(parsed, test = is_env,
                        envtypes = "tabular")
get_item(parsed, path)
}
