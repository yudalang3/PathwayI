% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class_Geometric.R
\name{create_point}
\alias{create_point}
\title{Note: for some situations, we need to define a formal point.
Thus, we could thinking like a object.
This is also support the vectorised usage.}
\usage{
create_point(x = 0, y = 0)
}
\arguments{
\item{x}{x value}

\item{y}{y value}
}
\value{
the point instance
}
\description{
Note: for some situations, we need to define a formal point.
Thus, we could thinking like a object.
This is also support the vectorised usage.
}
\examples{
create_point(1,2)
create_point(x = 1:10, y = 2:11)
}
