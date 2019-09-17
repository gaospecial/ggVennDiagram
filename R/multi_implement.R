#' Perform geometric set intersection with more than two simple feature geometry collections
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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
