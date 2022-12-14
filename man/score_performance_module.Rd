% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/default_plot_modules.R
\name{score_performance_module}
\alias{score_performance_module}
\alias{score_performance_ui}
\alias{score_performance_server}
\title{Input a column to generate a score performance plot}
\usage{
score_performance_ui(id)

score_performance_server(id, data, scores)
}
\arguments{
\item{id}{The namespace of the module.}

\item{data}{The data to plot.}

\item{scores}{The data frame of score specifications created by the user. See
\link{scores_init}.}
}
\value{
The server returns the selected column name.
}
\description{
A shiny module that allows the user to select a column to use for the score
performance plot.
}
\seealso{
\code{\link[=score_performance]{score_performance()}}
}
