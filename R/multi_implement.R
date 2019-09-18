#' Perform geometric set intersection with more than two simple feature geometry collections
#'
#' @param ... other arguments
#' @param l a list of items (polygon or vector)
#'
#' @return intersection/union/diff of items
#' @name multi
#'
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

#' Perform geometric set difference with more than two simple feature geometry collections
#' @rdname multi
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

#' Perform geometric set union with more than two simple feature geometry collections
#'
#' @rdname multi
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

#' Performs set union on more than two vectors.
#'
#' @rdname multi
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

#' Performs set intersection on more than two vectors.
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


#' Performs setdiff on more than two vectors.
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
