#' draw 2d, 3d, and 4d venn diagram
#'
#' @param x a list of items
#' @param n.sides  resolution
#' @param category.names default is names(x)
#' @param label c("both","percent","count")
#' @param ... passing to geom_polygon, enabling modification of polygon styles
#'
#' @name draw_venn
draw_4d_venn <- function(x, n.sides, category.names, label,...){

  category <- data.frame(x = c(0.08, 0.26, 0.71, 0.93),
                         y = c(0.78, 0.86, 0.85, 0.78),
                         label = category.names)

  region_data <- four_dimension_ellipse_regions(n.sides)


  counts <- four_dimension_region_values(x)

  plot_venn(region_data,category, counts, label, ...)


}

#' get the items in each region
#'
#' @param x a list of vector items
#'
#' @return  a list
#' @name region_item
four_dimension_region_items <- function(x){

  # values
  a <- x[[1]]
  b <- x[[2]]
  c <- x[[3]]
  d <- x[[4]]

  A <- setdiff(a,union(union(b,c),d))
  B <- setdiff(b,union(union(a,c),d))
  C <- setdiff(c,union(union(b,a),d))
  D <- setdiff(d,union(union(b,a),c))
  AB <- setdiff(intersect(a,b),union(c,d))
  AC <- setdiff(intersect(a,c),union(b,d))
  AD <- setdiff(intersect(a,d),union(c,b))
  BC <- setdiff(intersect(c,b),union(a,d))
  BD <- setdiff(intersect(d,b),union(c,a))
  CD <- setdiff(intersect(c,d),union(a,b))
  ABC <- setdiff(intersect(intersect(a,b),c),d)
  ABD <- setdiff(intersect(intersect(a,b),d),c)
  ACD <- setdiff(intersect(intersect(a,d),c),b)
  BCD <- setdiff(intersect(intersect(d,b),c),a)
  ABCD <- intersect(intersect(intersect(a,b),c),d)

  list(A=A,B=B,C=C,D=D,AB=AB,AC=AC,AD=AD,BC=BC,BD=BD,CD=CD,ABC=ABC,ABD=ABD,ACD=ACD,BCD=BCD,ABCD=ABCD)
}

#' calculating intersection values of venn
#'
#' @param x a list of vector items.
#'
#' @return data.frame
#' @name region_value
four_dimension_region_values <- function(x){

  items <- four_dimension_region_items(x)

  values <- sapply(items, length)
  data.frame(group=names(items),count=values,stringsAsFactors = F)
}

#' coordinations of polygon regions/centers for venn diagram
#'
#' @inheritParams draw_venn
#'
#' @importFrom VennDiagram ell2poly
#' @importFrom sf st_polygon st_difference st_intersection st_centroid st_union
#' @import dplyr
#' @export
#' @return a list of two data.frame, the first one represents polygon regions, and the second specifies polygon centers.
#' @name region_polygon
#' @examples
#' polygons <- four_dimension_ellipse_regions(3000)[[1]]
#' ggplot(polygons,aes(x,y,group=group,fill=group)) + geom_polygon()
four_dimension_ellipse_regions <- function(n.sides=1000){

  # ellipse
  parameters <- list(c(0.35, 0.47, 0.35, 0.20, 135),
                     c(0.50, 0.57, 0.35, 0.15, 135),
                     c(0.50, 0.57, 0.33, 0.15,  45),
                     c(0.65, 0.47, 0.35, 0.20,  45))
  ellipses <- lapply(parameters,function(x){
    do.call(ell2poly,as.list(c(x,n.sides))) %>%
      data.frame() %>%
      mutate(x=round(.data$x,6),y=round(.data$y,6))
  })

  polygons <- lapply(ellipses,function(x)st_polygon(list(as.matrix(x))))

  # regions
  A <- st_multi_difference(l=polygons)
  B <- st_multi_difference(l=polygons[c(2,1,3,4)])
  C <- st_multi_difference(l=polygons[c(3,1,2,4)])
  D <- st_multi_difference(l=polygons[c(4,1:3)])
  AB <- st_difference(st_intersection(polygons[[1]],polygons[[2]]),st_union(polygons[[3]],polygons[[4]]))
  AC <- st_difference(st_intersection(polygons[[1]],polygons[[3]]),st_union(polygons[[2]],polygons[[4]]))
  AD <- st_difference(st_intersection(polygons[[1]],polygons[[4]]),st_union(polygons[[3]],polygons[[2]]))
  BC <- st_difference(st_intersection(polygons[[3]],polygons[[2]]),st_union(polygons[[1]],polygons[[4]]))
  BD <- st_difference(st_intersection(polygons[[4]],polygons[[2]]),st_union(polygons[[3]],polygons[[1]]))
  CD <- st_difference(st_intersection(polygons[[3]],polygons[[4]]),st_union(polygons[[1]],polygons[[2]]))
  ABC <- st_difference(st_multi_intersection(l=polygons[1:3]),polygons[[4]])
  ABD <- st_difference(st_multi_intersection(l=polygons[c(1,2,4)]),polygons[[3]])
  ACD <- st_difference(st_multi_intersection(l=polygons[c(1,3,4)]),polygons[[2]])
  BCD <- st_difference(st_multi_intersection(l=polygons[c(4,2,3)]),polygons[[1]])
  ABCD <- st_multi_intersection(l=polygons)

  polygon_list <- list(A=A,B=B,C=C,D=D,AB=AB,AC=AC,AD=AD,BC=BC,BD=BD,CD=CD,ABC=ABC,ABD=ABD,ACD=ACD,BCD=BCD,ABCD=ABCD)
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
