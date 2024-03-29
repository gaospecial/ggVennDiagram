% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_data.R
\name{venn_plot_data}
\alias{venn_plot_data}
\alias{venn_regionedge}
\alias{venn_regionlabel}
\alias{venn_setedge}
\alias{venn_setlabel}
\alias{venn_set}
\alias{venn_region}
\title{Get VennPlotData slot}
\usage{
venn_regionedge(obj)

venn_regionlabel(obj)

venn_setedge(obj)

venn_setlabel(obj)

venn_set(obj)

venn_region(obj)
}
\arguments{
\item{obj}{a list that stores all the data from the S3 class `VennPlotData` object}
}
\value{
a tibble
}
\description{
Get VennPlotData slot
}
\examples{
venn = Venn(list(A=1:5,B=2:7,C=3:6,D=4:9))
obj = process_data(venn)
venn_regionlabel(obj)  # return regionLabel data
venn_regionedge(obj)   # return regionEdge data
venn_setlabel(obj) # return setLabel data
venn_setedge(obj)  # return setEdge data
venn_set(obj)     # set items
venn_region(obj)  # region items
}
