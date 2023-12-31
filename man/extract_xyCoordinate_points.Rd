% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class_Geometric.R
\name{extract_xyCoordinate_points}
\alias{extract_xyCoordinate_points}
\title{Extract xy Coordinate of points from a grid grob.}
\usage{
extract_xyCoordinate_points(grob, closed = T)
}
\arguments{
\item{grob}{The grob of the grid package.}

\item{closed}{Whether the grob is closed, default is T}
}
\value{
a 2xn matrix, where first row is the x axis and second row is the y axis.
}
\description{
Warnings: the curveGrob instance is not support, it will cause the problue
}
\examples{
extract_xyCoordinate_points(circleGrob, closed = F)
}
