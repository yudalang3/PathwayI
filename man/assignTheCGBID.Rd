% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class_TreeStructureInR6.R
\name{assignTheCGBID}
\alias{assignTheCGBID}
\title{Assign the CGB ID for all nodes.}
\usage{
assignTheCGBID(node)
}
\arguments{
\item{node:}{root node}
}
\value{
the minimum value ID among the children.
}
\description{
When the tree is generated, we assign the id according to the CGBID algorithm.

see https://doi.org/10.1093/bib/bbab583
}
\examples{
assignTheCGBID(rootNode)
}
