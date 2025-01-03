% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class_BioGraphicsObjs.R
\name{create_rectangular_node}
\alias{create_rectangular_node}
\title{Create the rectangular bio graphics node drawer.}
\usage{
create_rectangular_node(
  scaleWidth = 1,
  scaleHeight = 1,
  inner_extension_ratio = 0
)
}
\arguments{
\item{scaleWidth}{scaler for width}

\item{scaleHeight}{scaler for height}

\item{inner_extension_ratio}{try this argument yourself}
}
\value{
the instance
}
\description{
Create the rectangular bio graphics node drawer.
}
\details{
Note: this is the scaler of width height, also the final obtained height and width
Because the template's width and height both are the 1.
}
\examples{
create_rectangular_node()
}
