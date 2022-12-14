% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R
\name{score_summary}
\alias{score_summary}
\title{Generate a summary of a score}
\usage{
score_summary(x, ...)
}
\arguments{
\item{x}{The column being scored.}

\item{...}{The score arguments. See \code{\link[=create_score]{create_score()}}.}
}
\value{
A \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}} object containing the graph.
}
\description{
Generate a summary of a score currently being created. Generates a line graph
comparing the column value and the corresponding score.
}
\examples{
score_summary(
  1:100,
  score_type = "Linear", lb = 15, ub = 70, exponential = FALSE
)

}
