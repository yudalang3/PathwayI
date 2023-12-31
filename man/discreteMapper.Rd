% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class_ColorMapper.R
\name{discreteMapper}
\alias{discreteMapper}
\title{Generate discrete value to color mapper}
\usage{
discreteMapper(colors = NA, break_points = NA, colo_of_NA = "grey")
}
\arguments{
\item{colors}{colors}

\item{break_points}{break_points for the intervals}

\item{colo_of_NA}{color of NA}
}
\value{
colors have the same dim with the input
}
\description{
Generate discrete value to color mapper
}
\examples{

a <- discreteMapper(colors = c('red', 'blue'), break_points = c(1,2,3))
}
