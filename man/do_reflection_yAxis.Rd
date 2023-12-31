% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class_AffineTransformer.R
\name{do_reflection_yAxis}
\alias{do_reflection_yAxis}
\title{The core process to do the reflection affine transformation.
The reflection is about the y axis}
\usage{
do_reflection_yAxis(mat)
}
\arguments{
\item{mat}{the matrix that need to be transformed}
}
\value{
the transformed matrix. the row number is 2, not three.
}
\description{
The core process to do the reflection affine transformation.
The reflection is about the y axis
}
\examples{
mat <- rbind(c(1,0,1),c(2,1,0))
do_reflection_origin(mat)

}
