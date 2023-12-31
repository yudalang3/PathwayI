% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class_AffineTransformer.R
\name{produce_model_coordinate_points}
\alias{produce_model_coordinate_points}
\title{Produce the model circle points.}
\usage{
produce_model_coordinate_points(
  from = 0,
  by = 5,
  to = 90,
  break_points = NULL,
  radius = 1
)
}
\arguments{
\item{from}{: from angle in degree}

\item{by}{: by angle in degree}

\item{to}{: to angle in degree}

\item{break_points}{the angles points in degree}

\item{radius}{the radius of circle regardless of the coordinates.}
}
\value{
matrix, see details
}
\description{
It has many applications:
\enumerate{
\item produce poly regular shapes: like triangles, rectangular and so on.
\item just drawing a circle, with points.
}
}
\details{
The return values formats are:
a 2 x n matrix, first row is x axis, second row is y axis.

The 0 angle is 0 o'clock in the clock, angle of 90 is the 3 o'clock, angle of 180 is the 6 o'clock.
}
\examples{
a <- produce_model_coordinate_points(from = 0,to = 160,by = 30,radius = 1)
xCenter <- 3; yCenter <- 3
xLocations <- a[1, ] + xCenter;
yLocations <- a[2, ] + yCenter;

grid.newpage()
grid.lines(x = xLocations, y = yLocations, default.units = 'in')
}
