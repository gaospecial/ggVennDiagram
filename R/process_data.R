#' get plot data
#'
#' @param venn a Venn object
#' @param nsets This parameter will be set automatically.
#' @param shape_id apply filter to internal shapes. i.e. shape_id = "601"
#' @param type apply filter to internal shapes. i.e. type = "polygon"
#'
#' @details
#'
#' This function will conduct set operations and combine the outputs will
#' stored shapes, thus produce a dataset for plot in next step.
#'
#' Run `get_shapes()` to show all the characteristics of available shapes.
#' Run `plot_shapes()` to view those shapes.
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
          function(venn,
                   nsets = length(venn@sets),
                   shape_id = NULL,
                   type = NULL){
            shape = get_shape_data(nsets = nsets, shape_id = shape_id, type = type)
            plotData_add_venn(plotData = shape, venn = venn)
          })


#' Get VennPlotData slot
#'
#' @param obj a list that stores all the data from the S4 class `VennPlotData` object
#'
#' @return a tibble, `sf` object
#' @export
#' @name venn_plot_data
#'
#' @examples
#' venn_region(obj)   # return region data
#' venn_setlabel(obj) # return setLabel data
#' venn_setedge(obj)  # return setEdge data
venn_region <- function(obj){
  obj$region
}

#' @rdname venn_plot_data
#' @export
venn_setedge <- function(obj){
  obj$setEdge
}

#' @rdname venn_plot_data
#' @export
venn_setlabel <- function(obj){
  obj$setLabel
}


#' join the shape data with set data
#'
#' @param plotData a VennPlot object that stores plot shapes
#' @param venn a Venn object that stores set values
plotData_add_venn <- function(plotData, venn){
  if (!all(c("setLabel","setEdge","region") %in% names(plotData))) stop("Invalid shape data.")
  if (!inherits(venn, "Venn")) stop("venn should be a S4 Venn object.")
  edge_data <- process_setEdge_data(venn)
  label_data <- process_setLabel_data(venn)
  region_data <- process_region_data(venn)
  set_edge = get_shape_setedge(plotData)
  set_label = get_shape_setlabel(plotData)
  region = get_shape_region(plotData)
  plotData$setEdge <- dplyr::left_join(set_edge, edge_data, by = "id")
  plotData$setLabel <- dplyr::left_join(set_label, label_data, by = "id")
  plotData$region <- dplyr::left_join(region, region_data, by = "id")
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
  if(!inherits(venn, "Venn")) stop("venn is not a S4 class 'Venn' object.")
  tibble::tibble(
    id = as.character(seq_along(venn@sets)),
    item = venn@sets,
    count = sapply(venn@sets, length),
    name = venn@names
  )
}

#' @rdname venn_data
#' @export
process_setLabel_data <- function(venn){
  if(!inherits(venn, "Venn")) stop("venn is not a S4 class 'Venn' object.")
  tibble::tibble(
    id = as.character(seq_along(venn@sets)),
    name = venn@names
  )
}

#' @rdname venn_data
#' @export
process_region_data <- function(venn){
  if(!inherits(venn, "Venn")) stop("venn is not a S4 class 'Venn' object.")
  region_items <- get_region_items(venn)
  counts <- sapply(region_items, length)
  region_ids <- get_region_ids(venn)
  region_names <- get_region_names(venn)
  tibble::tibble(
    id = region_ids,
    item = region_items,
    count = counts,
    name = region_names
  )
}



