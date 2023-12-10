#' An S3 class constructor of representing Venn plot components.
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

#' S3 method for VennPlotData
#'
#' @method print VennPlotData
#' @export
print.VennPlotData = function(x){
  cat(sprintf("Class VennPlotData - '%s'\n", x$shapeId))
  cat(sprintf("  Number of sets: %d; Type: %s.\n", x$nsets, x$type))
  cat(sprintf("  To view this shape, use `plot_shape_edge()`.\n"))
  cat(sprintf("  To view its components, use `venn_setedge()`, `venn_setlabel()`, etc.\n"))
}

