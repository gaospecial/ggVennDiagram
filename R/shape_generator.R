#' functions to generate ellipse, circle, triangle and other shapes,
#'  which will be used in Venn plot
#' @name shape_generator
NULL



#' generating a closed ellipse
#'
#' This function is derived from `VennDiagram::ell2poly`, we modified it and then
#' it can generating a closed ellipse, which is a requirement for further transformation
#' to a POLYGON sf object.
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
#' library(ggVennDiagram)
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
#' @export
#'
#' @examples
#' # plot the default circle
#' library(ggVennDiagram)
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
#' @export
#' @return a matrix with xy coordinates
#'
#' @examples
#' # triangle coordinates
#' library(ggVennDiagram)
#' library(sf)
#' triangle()
#'
#' # plot a new triangle
#' triangle(c(-1,0,1,0,0,2)) %>% st_linestring() %>% plot()
triangle <- function(xy = c(0,0,1,0,0,1)){
  xy <- matrix(rep(xy, length.out =8), ncol=2, byrow = TRUE)
  colnames(xy) <- c("x","y")
  return(xy)
}


########## Four dimension ###########

#' fancy 4d ellipse from `VennDiagram`
#'
#' @param parameters will pass to shape generators
#' @param n count of points to shape this polygon
#'
#' @export
#'
#' @return a list of coordinates matrix
fancy_4d_ellipse <- function(parameters = NULL, n = 100){
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


#' @export
#' @rdname label_position
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

############## Three dimension circle #########

#' fancy 3d circle
#' @inheritParams fancy_4d_ellipse
#' @export
fancy_3d_circle <- function(parameters = NULL, n = 100){
  # three circles
  if(is.null(parameters))
    parameters <- list(c(0,0,4),c(4,0,4), c(2,-4,4))

  circles <- lapply(parameters, function(x){
    do.call(circle,as.list(c(x,n)))
  })

  circles
}

#' @export
#' @rdname label_position
fancy_3d_circle_label <- function(position = NULL){
  if (is.null(position))
    position <- tibble::tribble(
      ~x,       ~y,
      -3.5,     4.6,
      7.5,     4.6,
      2,      -8.5
    )
  label_position(position)
}

#' two dimension circle
#' @inheritParams fancy_4d_ellipse
#' @export
fancy_2d_circle <- function(parameters = NULL, n = 100){
  if(is.null(parameters))
    parameters <- list(c(0,0,4),c(0,4,4))

  circles <- lapply(parameters, function(x){
    do.call(circle,as.list(c(x,n)))
  })

  circles
}

#' @export
#' @rdname label_position
fancy_2d_circle_label <- function(position = NULL){
  if (is.null(position))
    position <- tibble::tribble(
      ~x,       ~y,
      -5,      0,
      -5,      4
    )
  label_position(position)
}

#' Six dimension triangle
#'
# triangles source: https://upload.wikimedia.org/wikipedia/commons/5/56/6-set_Venn_diagram_SMIL.svg
#' @inheritParams fancy_4d_ellipse
#' @export
fancy_6d_triangle <- function(parameters = NULL){
  if(is.null(parameters))
    parameters <- list(c(-69277,-32868,135580,121186, 70900,199427),
                   c( 81988,-44426, 38444,206222,121044,165111),
                   c(203271,  9619, 39604, 82683, 84652,206669),
                   c(333561,225349, 61764, 76805, 38980,182461),
                   c(131886,385785, 38136,111491, 94208, 24690),
                   c(-60184,274046,142476, 39903,103276,183962))

  shapes <- lapply(parameters, function(x){
    triangle(x)
  })

  shapes
}

#' @export
#' @rdname label_position
fancy_6d_triangle_label <- function(position = NULL){
  if (is.null(position))
    position <- tibble::tribble(
      ~x,       ~y,
      -50000,     50000,
      60000,          0,
      160000,     20000,
      280000,    170000,
      140000,    300000,
      -20000,   270000
    )
  label_position(position)
}

#' helper function to set label position
#'
#' @param position a data.frame containing label coordinates
#' @export
#'
#' @return a list of matrix
#' @name label_position
#'
#' @details
#' - `label_position`: basal wrapper for label postion
#' - `fancy_6d_triangle_label`: 6 sets triangle label position work with `fancy_6d_triangle`
#' - `fancy_4d_ellipse_label`: 4 sets ellipse label position work with `fancy_4d_ellipse`
#' - `fancy_3d_circle_label`: 3 sets circle label position work with `fancy_3d_circle`
#' - `fancy_2d_circle_label`: 2 sets circle label position work with `fancy_2d_circle`
#' @md
#'
#' @examples
#' fancy_4d_ellipse_label()
#' fancy_2d_circle_label()
label_position <- function(position){
  points <- lapply(seq_len(nrow(position)),function(i){
    as.matrix(position[i,])
  })

  points
}
