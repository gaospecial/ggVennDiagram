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
