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
#'  venn = Venn(list(A=1:3,B=2:5,C=4:8))
#'  data = process_data(venn)
#' }
setGeneric("process_data", function(venn, nsets = NULL, shape_id = NULL, type = NULL){
  standardGeneric("process_data")
})

#' @rdname process_data
#' @export
setMethod("process_data", signature = c(venn = "Venn"),
          function(venn,
                   nsets = length(venn@sets),
                   shape_id = NULL,
                   type = NULL){
            shape = get_shape_data(nsets = nsets, shape_id = shape_id, type = type)
            plotData_add_venn(plotData = shape, venn = venn)
          })


#' Get VennPlotData slot
#'
#' @param obj a list that stores all the data from the S3 class `VennPlotData` object
#'
#' @return a tibble
#' @export
#' @name venn_plot_data
#'
#' @examples
#' venn = Venn(list(A=1:5,B=2:7,C=3:6,D=4:9))
#' obj = process_data(venn)
#' venn_regionlabel(obj)  # return regionLabel data
#' venn_regionedge(obj)   # return regionEdge data
#' venn_setlabel(obj) # return setLabel data
#' venn_setedge(obj)  # return setEdge data
#' venn_set(obj)     # set items
#' venn_region(obj)  # region items
venn_regionedge = function(obj){
  if (!inherits(obj, "VennPlotData")) stop("obj should be a VennPlotData object.")
  obj$regionEdge |>
    dplyr::left_join(venn_region(obj), by = "id") |>
    dplyr::as_tibble()
}

#' @rdname venn_plot_data
#' @export
venn_regionlabel = function(obj){
  if (!inherits(obj, "VennPlotData")) stop("obj should be a VennPlotData object.")
  obj$regionLabel |> dplyr::as_tibble()
}

#' @rdname venn_plot_data
#' @export
venn_setedge = function(obj){
  if (!inherits(obj, "VennPlotData")) stop("obj should be a VennPlotData object.")
  obj$setEdge |> dplyr::as_tibble()
}

#' @rdname venn_plot_data
#' @export
venn_setlabel = function(obj){
  if (!inherits(obj, "VennPlotData")) stop("obj should be a VennPlotData object.")
  obj$setLabel |> dplyr::as_tibble()
}

#' @rdname venn_plot_data
#' @export
venn_set = function(obj){
  if (!inherits(obj, "VennPlotData")) stop("obj should be a VennPlotData object.")
  obj$setData |> dplyr::as_tibble()
}

#' @rdname venn_plot_data
#' @export
venn_region = function(obj){
  if (!inherits(obj, "VennPlotData")) stop("obj should be a VennPlotData object.")
  obj$regionData |> dplyr::as_tibble()
}

#' join the shape data with set data
#'
#' @param plotData a VennPlot object that stores plot shapes
#' @param venn a Venn object that stores set values
#'
#' @export
#'
plotData_add_venn = function(plotData, venn){
  if (!all(c("setLabel","setEdge","regionLabel", "regionEdge") %in% names(plotData))){
    stop("Invalid shape data.")
  }
  if (!inherits(venn, "Venn")) stop("venn should be a S4 Venn object.")
  set_data = process_set_data(venn)
  region_data = process_region_data(venn)
  set_edge = get_shape_setedge(plotData)
  set_label = get_shape_setlabel(plotData)
  region_edge = get_shape_regionedge(plotData)
  region_label = get_shape_regionlabel(plotData)
  plotData$setData = set_data
  plotData$setLabel = dplyr::left_join(set_label, set_data, by = "id")
  plotData$regionData = region_data
  plotData$regionLabel = dplyr::left_join(region_label, region_data, by = "id")
  return(plotData)
}

#' Prepare Venn data
#'
#' @name venn_data
#' @param venn a Venn object
#' @param sep name and id separator for intersections
#'
#' @return a tibble
#' @export
#'
#' @examples
#' x = list(
#' A = sample(letters, 8),
#' B = sample(letters, 8),
#' C = sample(letters, 8),
#' D = sample(letters, 8)
#' )
#'
#' venn = Venn(x)
#' process_set_data(venn)
#' process_region_data(venn)
process_set_data = function(venn){
  if(!inherits(venn, "Venn")) stop("venn is not a S4 class 'Venn' object.")
  tibble::tibble(
    id = as.character(seq_along(venn@sets)),
    name = venn@names,
    item = venn@sets,
    count = sapply(venn@sets, length)
  )
}

#' @rdname venn_data
#' @export
process_region_data = function(venn, sep = "/"){
  if(!inherits(venn, "Venn")) stop("venn is not a S4 class 'Venn' object.")
  region_items = get_subset_items(venn)
  counts = sapply(region_items, length)
  region_ids = get_subset_ids(venn, sep = sep)
  region_names = get_subset_names(venn, sep = sep)
  tibble::tibble(
    id = region_ids,
    name = region_names,
    item = region_items,
    count = counts
  )
}



