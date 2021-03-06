#' get plot data
#'
#' @param venn a Venn object
#' @param ... apply filter to internal shapes. i.e. shape_id == "601", type == "polygon"
#'
#' @name process_data
#' @docType methods
#' @export
#' @examples
#' \dontrun{
#'  venn <- Venn(list(A=1:3,B=2:5,C=4:8))
#'  data <- process_data(venn)
#' }
setGeneric("process_data", function(venn, ...) standardGeneric("process_data"))

#' @rdname process_data
#' @export
setMethod("process_data", signature = c("Venn"),
          function(venn, ...){
            shape <- get_shape_data(nsets = length(venn@sets), ...)
            plot_data <- VennPlotData(
              setEdge = dplyr::filter(shape, .data$component == "setEdge") %>% dplyr::pull(.data$xy),
              setLabel = dplyr::filter(shape, .data$component == "setLabel") %>% dplyr::pull(.data$xy)
            )
            plotData_add_venn(plotData = plot_data, venn = venn)
          })


#' Get VennPlotData slot
#'
#' @param obj a S4 class `VennPlotData` object
#'
#' @return a tibble, `sf` object
#' @export
#' @name venn_data
#'
#' @examples
#' \dontrun{
#' # obj is VennPlotData
#' venn_region(obj)   # return region data
#' venn_setlabel(obj) # return setLabel data
#' venn_setedge(obj)  # return setEdge data
#' }
venn_region <- function(obj){
  if (!inherits(obj, "VennPlotData")) stop(simpleError("object is not a S4 class 'VennPlotData'."))
  obj@region
}

#' @rdname venn_data
#' @export
venn_setedge <- function(obj){
  if (!inherits(obj, "VennPlotData")) stop(simpleError("object is not a S4 class 'VennPlotData'."))
  obj@setEdge
}

#' @rdname venn_data
#' @export
venn_setlabel <- function(obj){
  if (!inherits(obj, "VennPlotData")) stop(simpleError("object is not a S4 class 'VennPlotData'."))
  obj@setLabel
}

#' get applicable shape data for Venn object
#'
#' ggVennDiagram stores shapes as internal data. You may see all the shapes by
#' using `plot_shapes()`.
#'
#' @param nsets number of sets
#' @inheritDotParams process_data
#'
#' @return a tibble describing specific shape
#' @export
#'
#' @examples
#' get_shape_data(nsets = 3, type == "polygon")
#'
get_shape_data <- function(nsets, ...){
  data <- shapes %>% dplyr::filter(.data$nsets == {{nsets}}, ...)
  if (length(unique(data[["shape_id"]]))>1) {
    # message("More than one shapes are available for ", n, " sets Venn plot. ",
    #          "Will choose the first one.\n",
    #         "You may explictly select a shape by specify shape_id, and/or type.\n\n")
    the_id <- unique(data[["shape_id"]])[1]
    data <- data %>% dplyr::filter(.data$shape_id == the_id)
  }

  data
}

#' join the shape data with set data
#'
#' @param plotData a VennPlot object that stores plot shapes
#' @param venn a Venn object that stores set values
#'
#' @export
plotData_add_venn <- function(plotData, venn){
  if (!inherits(plotData,"VennPlotData")) stop(simpleError("plotData should be a S4 VennPlotData object."))
  if (!inherits(venn, "Venn")) stop(simpleError("venn should be a S4 Venn object."))
  edge_data <- process_setEdge_data(venn)
  label_data <- process_setLabel_data(venn)
  region_data <- process_region_data(venn)
  plotData@setEdge <- dplyr::left_join(plotData@setEdge, edge_data, by = "id")
  plotData@setLabel <- dplyr::left_join(plotData@setLabel, label_data, by = "id")
  plotData@region <- dplyr::left_join(plotData@region, region_data, by = "id")
  return(plotData)
}

#' Prepare Venn data
#'
#' @name venn_data
#' @param venn a Venn object
#'
#' @return a tibble
#' @export
#'
#' @examples
#' x <- list(
#' A = sample(letters, 8),
#' B = sample(letters, 8),
#' C = sample(letters, 8),
#' D = sample(letters, 8)
#' )
#'
#' venn <- Venn(x)
#' process_region_data(venn)
#' process_setEdge_data(venn)
#' process_setLabel_data(venn)
process_setEdge_data <- function(venn){
  if(!inherits(venn, "Venn")) stop(simpleError("venn is not a S4 class 'Venn' object."))
  tibble::tibble(
    component = "setEdge",
    id = as.character(seq_along(venn@sets)),
    item = venn@sets,
    count = sapply(venn@sets, length),
    name = venn@names
  )
}

#' @rdname venn_data
#' @export
process_setLabel_data <- function(venn){
  if(!inherits(venn, "Venn")) stop(simpleError("venn is not a S4 class 'Venn' object."))
  tibble::tibble(
    component = "setLabel",
    id = as.character(seq_along(venn@sets)),
    name = venn@names
  )
}

#' @rdname venn_data
#' @export
process_region_data <- function(venn){
  if(!inherits(venn, "Venn")) stop(simpleError("venn is not a S4 class 'Venn' object."))
  region_items <- get_region_items(venn)
  counts <- sapply(region_items, length)
  region_ids <- get_region_ids(venn)
  region_names <- get_region_names(venn)
  tibble::tibble(
    component = "region",
    id = region_ids,
    item = region_items,
    count = counts,
    name = region_names
  )
}



