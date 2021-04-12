#' An S4 class to represent multiple polygons.
#'
#' @slot sets A `list` object contains sets
#' @slot names The names of the `sets` if has names. If the `list`
#'   doesn't have names, the sets will be named as "Set_1", "Set_2"
#'   and so on.
#' @import RVenn
setClass("Polygon",
         slots = list(sets = "ANY", names = "ANY"),
         contains = "Venn")

setGeneric("Polygon", function(sets){
  standardGeneric("Polygon")
})

#' @export
#' @importFrom methods new
setMethod("Polygon", c(sets = "ANY"),
          function(sets){
            if (!is.list(sets)){
              stop("Data sets should be a list.")
            }

            if (sum(sapply(sets, is.null) == TRUE) >= 1){
              sets = sets[!(sapply(sets, is.null))]
            }

            if (length(sets) <= 1){
              stop("The list should contain at least 2 vectors.")
            }

            if (length(unique(lapply(sets, class))) != 1) {
              stop("Vectors should be in the same class.")
            }

            if (!(sapply(sets, class)[1] %in% c("XY", "POLYGON", "sfg"))) {
              stop("The list must contain only XY, POLYGON or sfg object.")
            }

            polygon = new(Class = "Polygon", sets = sets)

            if (is.null(names(polygon@sets))) {
              names(polygon@sets) = paste("Set", seq_len(length(polygon@sets)), sep = "_")
            }

            polygon@names = names(polygon@sets)

            polygon
          })


#' An S4 class to represent Venn plot components.
#'
#' @slot edge
#' @slot region
#' @slot label
setClass("VennPlotData",
         slots = list(setEdge = "ANY", setLabel = "ANY", region = "ANY"))

setGeneric("VennPlotData", function(setEdge){
  standardGeneric("VennPlotData")
})

#' @export
#' @importFrom methods new
setMethod("VennPlotData", c(setEdge = "ANY", setLabel = "ANY", region = "ANY"),
          function(setEdge, setLabel = NULL, region = "auto"){
            if (!is.list(sets)){
              stop("Data sets should be a list.")
            }

            if (sum(sapply(sets, is.null) == TRUE) >= 1){
              sets = sets[!(sapply(sets, is.null))]
            }

            if (length(sets) <= 1){
              stop("The list should contain at least 2 vectors.")
            }

            if (length(unique(lapply(sets, class))) != 1) {
              stop("Vectors should be in the same class.")
            }

            if (!(sapply(sets, class)[1] %in% c("XY", "POLYGON", "sfg"))) {
              stop("The list must contain only XY, POLYGON or sfg object.")
            }

            data = new(Class = "VennPlotData", setEdge = setEdge)

            data
          })
