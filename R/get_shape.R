

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
#' get_shape_data(nsets = 4, type = "polygon")
get_shape_data <- function(nsets, type = NULL, shape_id = NULL){
  all_shape = get_shapes()
  data = all_shape[all_shape$nsets == nsets,]
  if (!is.null(type)) data = data[data$type == type, ]
  if (!is.null(shape_id)) data = data[data$shape_id == shape_id, ]
  if (nrow(data)>1) {
    # message("More than one shapes are available for ", n, " sets Venn plot. ",
    #          "Will choose the first one.\n",
    #         "You may explictly select a shape by specify shape_id, and/or type.\n\n")
    data = data |> dplyr::arrange(.data$type)
    the_id = data[["shape_id"]][[1]]
    return(get_shape_by_id(the_id))

  } else if (nrow(data) == 1) {
    return(get_shape_by_id(data$shape_id))
  } else {
    stop("No available shape are found under your settings.",
         " Run 'get_shapes()' to see all the available shapes.")
  }
}


#' Get all shapes
#'
#' @return a tibble
#' @export
#'
#' @examples
#' get_shapes()
get_shapes = function(){
  df = lapply(shapes, function(x){
    tibble::tibble(shape_id = get_shape_id(x),
                   nsets = get_shape_nsets(x),
                   type = get_shape_type(x))
  }) |> dplyr::bind_rows()
  return(df)
}

#' Specifying a shape
#'
#' @param id shape id
#'
#' @return a shape
#' @export
#'
#' @examples
#' get_shape_by_id("401f")
get_shape_by_id = function(id){
  idx = sapply(shapes, function(x) get_shape_id(x) == id)
  if (sum(idx) == 1) return(shapes[idx][[1]])
}

get_shape_id = function(x){
  x$shapeId
}

get_shape_type = function(x){
  x$type
}

get_shape_nsets = function(x){
  x$nsets
}

get_shape_setedge = function(x, ...){
  df = x$setEdge
  extra = list(...)
  extra$id = as.character(seq_len(x$nsets))
  extra = as.data.frame(extra)
  df |> dplyr::left_join(extra, by = "id")
}

get_shape_setlabel = function(x, ...){
  df = x$setLabel
  extra = list(...)
  extra$id = as.character(seq_len(x$nsets))
  extra = as.data.frame(extra)
  df |> dplyr::left_join(extra, by = "id")
}

get_shape_regionedge = function(x){
  x$regionEdge
}

get_shape_regionlabel = function(x){
  x$regionLabel
}
