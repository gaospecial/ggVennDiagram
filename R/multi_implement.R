#' Perform geometric set intersection, difference, and union with more than two simple feature geometry collections
#'
#' @param ... at least three items are needed if use this parameter
#' @param l a list of polygons
#'
#' @return intersection/union/diff of items
#' @name multi_st_fun
st_multi_intersection <- function ( ...,l = NULL){
  if (is.null(l)) l <- list(...)
  n <- length(l)
  if (n < 3){
    stop("At least three parameters.")
  }
  else {
    v <- l[[1]]
    for (i in 2:n){
      v <- st_intersection(v,l[[i]])
    }
    return(v)
  }
}

#' @rdname multi_st_fun
st_multi_difference <- function ( ...,l = NULL){
  if (is.null(l)) l <- list(...)
  n <- length(l)
  if (n < 3){
    stop("At least three parameters.")
  }
  else {
    v <- l[[1]]
    for (i in 2:n){
      v <- st_difference(v,l[[i]])
    }
    return(v)
  }
}


#' @rdname multi_st_fun
st_multi_union <- function ( ...,l = NULL){
  if (is.null(l)) l <- list(...)
  n <- length(l)
  if (n < 3){
    stop("At least three parameters.")
  }
  else {
    v <- l[[1]]
    for (i in 2:n){
      v <- st_union(v,l[[i]])
    }
    return(v)
  }
}

#' Performs set union/intersection/diff on more than two vectors.
#' @param ... at least three items are needed if use this parameter
#' @param l a list of vectors
#'
#' @name multi
multi_union <- function ( ...,l = NULL){
  if (is.null(l)) l <- list(...)
  n <- length(l)
  if (n < 3){
    stop("At least three parameters.")
  }
  else {
    v <- l[[1]]
    for (i in 2:n){
      v <- union(v,l[[i]])
    }
    return(v)
  }
}

#' @rdname multi
multi_intersect <- function ( ...,l = NULL){
  if (is.null(l)) l <- list(...)
  n <- length(l)
  if (n < 3){
    stop("At least three parameters.")
  }
  else {
    v <- l[[1]]
    for (i in 2:n){
      v <- intersect(v,l[[i]])
    }
    return(v)
  }
}


#' @rdname multi
multi_setdiff <- function(...,l=NULL){
  if (is.null(l)) l <- list(...)
  n <- length(l)
  if (n < 3){
    stop("At least three parameters.")
  }
  else {
    v <- l[[1]]
    for (i in 2:n){
      v <- setdiff(v,l[[i]])
    }
    return(v)
  }
}
