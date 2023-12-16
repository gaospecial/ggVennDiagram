#' An S3 class constructor of representing Venn plot components.
#'
#' @param x data source of a VennPlotData object
#'
#' @slot shapeId shape id
#' @slot type type of shape
#' @slot nsets number of sets
#' @slot setEdge a data.frame, the coordinates of set edges, can be retrieved by `venn_setedge()`
#' @slot setLabel a data.frame, the coordinates of set labels, can be retrieved by `venn_setlabel()`
#' @slot regionEdge a data.frame, the coordinates of different regions, can be retrieved by `venn_regionedge()`
#' @slot regionLabel a data.frame, the centroid of the regions, where region labels anchored, can be retrieved by `venn_regionlabel()`
#' @slot setData a data.frame, the set data provided by user, can be retrieved by `venn_set()`
#' @slot regionData a data.frame, the region data that calculated by `ggVennDiagram`, can be retrieved by `venn_region()`
#' @md
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
#' @rdname print
#' @name print
#' @md
#' @export
print.VennPlotData = function(x, ...){
  cat(sprintf("Class VennPlotData - '%s'\n", x$shapeId))
  cat(sprintf("  Type: %s; No. sets: %d; No. regions: %d.\n", x$type, x$nsets, length(unique(x$regionEdge$id))))
  cat(sprintf("  To view this shape, use `plot_shape_edge()`.\n"))
  cat(sprintf("  To view its components, use `venn_setedge()`, `venn_setlabel()`, etc.\n"))
}

