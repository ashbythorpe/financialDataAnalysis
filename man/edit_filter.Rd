% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_filter.R
\name{edit_filter}
\alias{edit_filter}
\title{Edit a filter in a table of filters}
\usage{
edit_filter(filters, x, pattern = NULL, min = NULL, max = NULL)
}
\arguments{
\item{filters}{A data frame of filters. See \link{filters_init}.}

\item{x}{A number specifying the filter number to edit (indexed at 1).}

\item{pattern}{A string.}

\item{min}{A number.}

\item{max}{A number.}
}
\value{
\code{filters} with an edited row, provided that the necessary arguments are
valid.
}
\description{
Edit a filter for a data frame, by editing a row of a data frame, where each
row represents a filter. If your edit results in an invalid filter, the edit
does not succeed.
}
\examples{
data <- tibble::tibble(
  x = 1:10,
  y = "a"
)

filters <- add_filter(filters_init, "x", data)
edit_filter(filters, 1, min = 1, max = 5)

}
\seealso{
\itemize{
\item \link{filters_init} for the filters table.
\item \code{\link[=add_filter]{add_filter()}} and \code{\link[=remove_filter]{remove_filter()}} to further edit the filters table.
\item \code{\link[=apply_filters]{apply_filters()}} to apply the filters you have created to your data.
}
}
