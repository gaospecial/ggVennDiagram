% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SetOperations.R
\name{discern_overlap}
\alias{discern_overlap}
\alias{discern_overlap,Venn-method}
\alias{discern_overlap,Polygon-method}
\title{calculate region of Venn}
\usage{
discern_overlap(venn, slice = "all")

\S4method{discern_overlap}{Venn}(venn, slice = "all")

\S4method{discern_overlap}{Polygon}(venn, slice = "all")
}
\arguments{
\item{venn}{Venn object}

\item{slice}{a numeric vector indicating the index of slice, default is "all"}
}
\value{
region items
}
\description{
calculate region of Venn

calculate the unique region defined by `Venn` object and the parameter `slice`
}
\examples{
library(ggVennDiagram)
venn <- Venn(list(A=1:3,B=2:5,C=c(1L,3L,5L)))

discern_overlap(venn, slice = "all")
# is equal to
overlap(venn, slice = "all")

# however, `discern_overlap()` only contains specific region
discern_overlap(venn, slice = 1:2)

}
