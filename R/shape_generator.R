#' functions to generate ellipse, circle, triangle and other shapes,
#'  which will be used in Venn plot
#' @name shape_generator
NULL


# modified from VennDiagram::ell2poly
#' generating a ellipse
#'
#' @param x,y the coordinates of ellipse center
#' @param a radius of short arm
#' @param b radius of long arm
#' @param rotation rotation in degree
#' @param n number of points
#'
#' @return a matrix representing ellipse coordinates
#' @export
#'
#' @examples
#' # plot the default ellipse
#' library(sf)
#' ellipse() %>% st_linestring() %>% plot()
ellipse <- function(x = 0, y =0, a = 2, b = 1, rotation = 0, n = 100){
  rotation <- rotation * pi/180
  theta <- 2 * pi/n
  angles <- seq(0, 2 * pi, theta)
  x.coord <- vector(length = n+1, mode = "numeric")
  y.coord <- vector(length = n+1, mode = "numeric")
  for (i in 1:n) {
    x.coord[i] <- x + a * cos(angles[i]) * cos(rotation) -
      b * sin(angles[i]) * sin(rotation)
    y.coord[i] <- y + a * cos(angles[i]) * sin(rotation) +
      b * sin(angles[i]) * cos(rotation)
  }
  # close ellipse
  x.coord[n+1] <- x.coord[1]
  y.coord[n+1] <- y.coord[1]

  as.matrix(data.frame(x = x.coord, y = y.coord))
}


#' generating a circle
#'
#' @param x,y center of circle
#' @param r radius of circle
#' @param n number of points for polygon object (resolution)
#'
#' @return a matrix representing circle coordinates
#' @examples
#' # plot the default circle
#' library(sf)
#' circle() %>% st_linestring() %>% plot()
circle <- function(x = 0, y = 0, r = 1, n=100){
  angles <- seq(0,2*pi,length.out = n)
  x.coord <- x + cos(angles) * r
  y.coord <- y + sin(angles) * r
  x.coord[n] <- x.coord[1]
  y.coord[n] <- y.coord[1]
  as.matrix(data.frame(x=x.coord,y=y.coord))
}

#' defined a triangle by three points
#'
#' @param xy coordinates of the three points defining a triangle
#'
#' @return
#' @export
#'
#' @examples
#' # triangle coordinates
#' triangle()
#'
#' # plot a new triangle
#' triangle(c(-1,0,1,0,0,2)) %>% st_linestring() %>% plot()
triangle <- function(xy = c(0,0,1,0,0,1)){
  xy <- matrix(rep(xy, length.out =8), ncol=2, byrow = TRUE)
  return(xy)
}



#' fancy 4d ellipse from `VennDiagram`
#'
#' @return a list of coordinates matrix
fancy_4d_ellipse <- function(parameters = NULL, n = 50){
  # 4d ellipses
  if (is.null(parameters))
    parameters <- list(c(0.35, 0.47, 0.35, 0.20, 135),
                     c(0.50, 0.57, 0.35, 0.15, 135),
                     c(0.50, 0.57, 0.33, 0.15,  45),
                     c(0.65, 0.47, 0.35, 0.20,  45))
  ellipses <- lapply(parameters,function(x){
    ellipse(x[[1]],x[[2]],x[[3]],x[[4]],x[[5]],n = n)
  })

  ellipses
}




fancy_3d_circle <- function(parameters = NULL, n = 100){
  # three circles
  if(is.null(parameters))
    parameters <- list(c(2,4,4),c(0,0,4),c(4,0,4))

  circles <- lapply(parameters, function(x){
    do.call(circle,as.list(c(x,n)))
  })

  circles
}

#' helper function to set label position
#'
#' @param position a data.frame containing label coordinates
#'
#' @return
#' @export
#'
#' @examples
label_position <- function(position){
  points <- lapply(seq_len(nrow(position)),function(i){
    as.matrix(label_position[i,])
  })

  points
}

fancy_4d_ellipse_label <- function(position = NULL){
  if (is.null(position))
    position <-  tibble::tribble(
        ~x,       ~y,
      0.08,      0.78,
      0.26,      0.86,
      0.71,      0.85,
      0.93,      0.78
    )
  label_position(position)

}

fancy_3d_circle_label <- function(position){
  if (is.null(position))
    position <- tibble::tribble(
        ~x,       ~y,
         2,      8.5,
      -3.5,     -4.6,
       7.5,     -4.6
    )
  label_position(position)
}
