#' An S3 class constructor of representing Venn plot components.
#'
#' @param x data source of a VennPlotData object
#'
#' @slot shapeId shape id
#' @slot type type of shape
#' @slot nsets number of sets
#' @slot setEdge a list of coordinates matrix defining Venn set edges
#' @slot setLabel label of sets
#' @slot regionEdge the feature region will be calculated automatically with `setEdge`
#' @slot regionLabel the centroid of region, where region label appears
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

