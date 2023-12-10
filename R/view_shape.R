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
  cat("print")
}

