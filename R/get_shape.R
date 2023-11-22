

#' get applicable shape data for Venn object
#'
#' ggVennDiagram stores shapes as internal data. You may see all the shapes by
#' using `plot_shapes()` or `get_shapes()`.
#'
#' @param type type of shape
#' @param shape_id shape id
#' @param nsets number of sets
#'
#' @return a tibble describing specific shape
#' @export
#'
#' @examples
#' get_shape_data(nsets = 3, type == "polygon")
#'
get_shape_data <- function(nsets, type = NULL, shape_id = NULL){
  all_shape = get_shapes()
  data = dplyr::filter(all_shape, .data$nsets == nsets)
  if (!is.null(type)) data = dplyr::filter(data, .data$type == type)
  if (!is.null(id)) data = dplyr::filter(data, .data$shape_id == shape_id)
  if (nrow(data)>1) {
    # message("More than one shapes are available for ", n, " sets Venn plot. ",
    #          "Will choose the first one.\n",
    #         "You may explictly select a shape by specify shape_id, and/or type.\n\n")
    the_id <- data[["shape_id"]][1]
    return(get_shape_by_id(the_id))

  } else if (nrow(data) == 1) {
    return(get_shape_by_id(data$shape_id))
  } else {
    warning("No available shape are found under your settings.")
    print(all_shape)
    invisible(stop())
  }
}


get_shapes = function(){
  df = lapply(shapes, function(x){
    tibble::tibble(shape_id = get_shape_id(x),
                   nsets = get_shape_nsets(x),
                   type = get_shape_type(x))
  }) |> dplyr::bind_rows()
  return(df)
}

get_shape_by_id = function(id){
  idx = sapply(shapes, function(x) get_shape_id(x) == id)
  if (sum(idx) == 1) return(shapes[idx][[1]])
}

get_shape_id = function(x){
  x@shapeId
}

get_shape_type = function(x){
  x@type
}

get_shape_nsets = function(x){
  x@nsets
}

get_shape_setedge = function(x){
  x@setEdge
}

get_shape_region = function(x){
  x@region
}