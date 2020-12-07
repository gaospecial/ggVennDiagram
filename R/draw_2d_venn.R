#' @rdname draw_venn
draw_2d_venn <- function(x, n.sides, category.names, label,...){
  category <- data.frame(x = c(-1, 5),
                         y = 4.3,
                         label = category.names)

  region_data <- two_dimension_circle_regions(n.sides)

  counts <- two_dimension_region_values(x)

  plot_venn(region_data,category, counts, label, ...)
}

#' @rdname region_polygon
#' @export
two_dimension_circle_regions <- function(n.sides=1000){
  # two circles
  parameters <- list(c(0,0,4),c(4,0,4))

  circles <- lapply(parameters, function(x){
    do.call(circle,as.list(c(x,n.sides)))
  })

  polygons <- lapply(circles, function(x)st_polygon(list(as.matrix(x))))

  # regions
  A <- st_difference(polygons[[1]],polygons[[2]])
  B <- st_difference(polygons[[2]],polygons[[1]])
  AB <- st_intersection(polygons[[1]],polygons[[2]])

  polygon_list <- list(A=A,B=B,AB=AB)
  polygon_name <- names(polygon_list)
  polygon_dfs <- lapply(1:length(polygon_list), function(i){
    df <- unlist(polygon_list[[i]]) %>% matrix(ncol = 2) %>% data.frame()
    colnames(df) <- c("x","y")
    df$group <- polygon_name[[i]]
    return(df)
  })
  data_ploygons <- do.call(rbind,polygon_dfs)

  # centers
  data_centers <- lapply(polygon_list, st_centroid) %>% unlist %>% matrix(byrow = T,ncol=2) %>% data.frame()
  data_centers$group <- polygon_name
  colnames(data_centers) <- c("x","y","group")

  list(data_ploygons, data_centers)
}

#' @rdname region_item
two_dimension_region_items <- function(x){
  a <- x[[1]]
  b <- x[[2]]

  A <- setdiff(a, b)
  B <- setdiff(b, a)
  AB <- intersect(a,b)

  list(A=A,B=B,AB=AB)
}

#' @rdname region_value
two_dimension_region_values <- function(x){
  items <- two_dimension_region_items(x)

  region_values(items)

}
