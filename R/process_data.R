# process plot data for venn
setGeneric("process_data", function(venn, ...) standardGeneric("process_data"))

#' get plot data
#'
#' @param venn a Venn object
#' @param ... filter shapes. i.e. shape_id == "601", type == "polygon"
#'
#' @export
#' @name process_data
setMethod("process_data", signature = c("Venn"),
          function(venn, ...){
            shape <- get_shape_data(nsets = length(venn@sets), ...)
            plot_data <- VennPlotData(
              setEdge = dplyr::filter(shape, component == "setEdge") %>% dplyr::pull(xy),
              setLabel = dplyr::filter(shape, component == "setLabel") %>% dplyr::pull(xy)
            )
            plotData_add_venn(plotData = plot_data, venn = venn)
          })

#' get applicable shape data for Venn object
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

process_setEdge_data <- function(venn){
  tibble::tibble(
    component = "setEdge",
    id = as.character(seq_along(venn@sets)),
    count = sapply(venn@sets, length),
    name = venn@names
  )
}

process_setLabel_data <- function(venn){
  tibble::tibble(
    component = "setLabel",
    id = as.character(seq_along(venn@sets)),
    name = venn@names
  )
}

process_region_data <- function(venn){
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



