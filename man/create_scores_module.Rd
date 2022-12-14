% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_scores_module.R
\name{create_scores_module}
\alias{create_scores_module}
\alias{create_scores_ui}
\alias{create_scores_server}
\title{Create score specifications}
\usage{
create_scores_ui(id)

create_scores_server(id, data, interactive)
}
\arguments{
\item{id}{The namespace of the module.}

\item{data}{The data to score.}

\item{interactive}{Whether any plots created should be interactive.}
}
\value{
The server returns the table of score specifications that the user has
created.
}
\description{
A shiny module that contains the Create Scores page, allowing the user to
create a set of score specifications. These are then automatically applied
to the data.
}
