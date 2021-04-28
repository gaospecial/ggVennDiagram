#' An S4 class to represent multiple sets.
#'
#' This class is adopted from RVenn. Since RVenn doesn't export this class,
#' I have to copy its codes hereafter.
#'
#' @slot sets A \code{list} object containing vectors in the same type.
#' @slot names The names of the \code{sets} if it has names. If the \code{list}
#'   doesn't have names, the sets will be named as "Set_1", "Set_2", "Set_3" and
#'   so on.
#' @name Venn-class
setClass("Venn",
         slots = list(sets = "ANY", names = "ANY")
)


#' @export
#' @import RVenn
setGeneric("Venn", getGeneric("Venn",package = "RVenn"))

#' An S4 class to represent multiple polygons.
#'
#' @slot sets A list contains sets
#' @slot names The names of the `sets` if has names. If the `list`
#'   doesn't have names, the sets will be named as "Set_1", "Set_2"
#'   and so on.
#'
setClass("Polygon",
         slots = list(sets = "ANY", names = "ANY"),
         contains = "Venn")


#' Polygon constructor
#'
#' @param sets a list containing multiple simple features
#' @export
#' @docType methods
#' @rdname polygon-methods
setGeneric("Polygon", function(sets){
  standardGeneric("Polygon")
})


#' @rdname polygon-methods
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
#' @slot setEdge a list of coordinates matrix defining Venn set edges
#' @slot SetLabel a list of coordinates matrix defining Venn set labels
#' @slot region the feature region will be calculated automatically with `setEdge`
#'
setClass("VennPlotData",
         slots = list(setEdge = "ANY", setLabel = "ANY", region = "ANY"))

#' VennPlotData constructor
#'
#' @param setEdge a list of coordinates matrix defining Venn set edges
#' @param setLabel a list of coordinates matrix defining Venn set labels#'
#' @return a S4 class VennPlotData object
#'
#' @name VennPlotData
#' @docType methods
setGeneric("VennPlotData", function(setEdge, setLabel){
  standardGeneric("VennPlotData")
})



#' @rdname VennPlotData
#' @export
#' @importFrom methods new
setMethod("VennPlotData", c(setEdge = "ANY", setLabel = "ANY"),
          function(setEdge, setLabel){
            if (!is.list(setEdge) | !is.list(setLabel))
              stop("SetEdge/setLabel must be a list.")
            if (length(setEdge) != length(setLabel))
              stop("SetEdge/setlabel must be the same length.")
            if (!all(sapply(setEdge, is.matrix), sapply(setLabel, is.matrix)))
              stop("The element in setEdge/setLabel list must be a matrix.")

            edge <- .setEdge(setEdge)
            label <- .setLabel(setLabel)
            region <- .region(setEdge)
            data = new(Class = "VennPlotData", setEdge = edge, setLabel = label, region = region)
            data
          })

.setEdge <- function(setEdge){
  linestrings <- lapply(setEdge, sf::st_linestring)
  d <- tibble::tibble(
    id = as.character(seq_len(length(setEdge))),
    geometry = linestrings
  )
  sf::st_as_sf(d)
}

.setLabel <- function(setLabel){
  points <- lapply(setLabel, sf::st_point)
  d <- tibble::tibble(
    id = as.character(seq_len(length(setLabel))),
    geometry = points
  )
  sf::st_as_sf(d)
}

.region <- function(setEdge){
  polygons <- lapply(setEdge, function(x) sf::st_polygon(list(x)))
  polygon <- Polygon(polygons)
  regions <- get_region_items(polygon)
  region_id <- get_region_ids(polygon)
  d <- tibble::tibble(
    id = region_id,
    geometry = regions
  )
  sf::st_as_sf(d)
}





