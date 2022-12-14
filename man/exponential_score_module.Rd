% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_input_modules.R
\name{exponential_score_module}
\alias{exponential_score_module}
\alias{exponential_score_ui}
\alias{exponential_score_server}
\title{Input the exponential transformation arguments}
\usage{
exponential_score_ui(id)

exponential_score_server(id, reset, editing_row)
}
\arguments{
\item{id}{The namespace of the module.}

\item{reset}{When this value is changed, all inputs will reset.}

\item{editing_row}{The row that the user is currently editing. When this
changes, all inputs will be updated to the state of the score that is being
edited.}
}
\value{
A \code{\link[tibble:tibble]{tibble::tibble_row()}} containing the four inputs, or NULL if any are
invalid.
}
\description{
A shiny module that allows the user to input the score arguments that decide
if and how an exponential transformation will be performed on a score
(exponential, logarithmic and magnitude).
}
