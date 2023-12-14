#' An S3 class constructor of representing Venn plot components.
#'
#' @param x data source of a VennPlotData object
#'
#' @slot shapeId shape id
#' @slot type type of shape
#' @slot nsets number of sets
#' @slot setEdge a data.frame, the coordinates of set edges
#' @slot setLabel a data.frame, the coordinates of set labels
#' @slot regionEdge a data.frame, the coordinates of different regions
#' @slot regionLabel a data.frame, the centroid of the regions, where region labels anchored
VennPlotData = function(x){
  class(x) = "VennPlotData"
  return(x)
}

#' S3 method for `VennPlotData`
#'
#' @param x a VennPlotData object
#' @param ... useless
#'
#' @method print VennPlotData
#' @docType methods
#' @name print
#' @md
#' @export
print.VennPlotData = function(x, ...){
  cat(sprintf("Class VennPlotData - '%s'\n", x$shapeId))
  cat(sprintf("  Type: %s; No. sets: %d; No. regions: %d.\n", x$type, x$nsets, length(unique(x$regionEdge$id))))
  cat(sprintf("  To view this shape, use `plot_shape_edge()`.\n"))
  cat(sprintf("  To view its components, use `venn_setedge()`, `venn_setlabel()`, etc.\n"))
}

