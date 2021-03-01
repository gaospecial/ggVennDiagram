#' draw Venn diagram
#'
#' @param x a list of items
#' @param n.sides  resolution
#' @param category.names default is names(x)
#' @param label c("both","percent","count")
#' @param ... passing to geom_polygon, enabling modification of polygon styles
#'
#' @name draw_venn
draw_6d_venn <- function(x, n.sides, category.names, label,...){

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
six_dimension_region_items <- function(x){

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
six_dimension_region_values <- function(x){

  items <- six_dimension_region_items(x)

  region_values(items)
}



#' Coordinates of polygon regions/centers for venn diagram
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
#' library(ggplot2)
#' polygons <- four_dimension_ellipse_regions(3000)[[1]]
#' ggplot(polygons,aes(x,y,group=group,fill=group)) + geom_polygon()
six_dimension_ellipse_regions <- function(n.sides=1000){

  # triangles source: https://upload.wikimedia.org/wikipedia/commons/5/56/6-set_Venn_diagram_SMIL.svg
  parameters <- list(c(-69277,-32868,135580,121186, 70900,199427),
                     c( 81988,-44426, 38444,206222,121044,165111),
                     c(203271,  9619, 39604, 82683, 84652,206669),
                     c(333561,225349, 61764, 76805, 38980,182461),
                     c(131886,385785, 38136,111491, 94208, 24690),
                     c(-60184,274046,142476, 39903,103276,183962)
                     )

  polygons <- lapply(parameters,function(x){
    st_polygon(list(matrix(rep(x, length.out =8), ncol=2, byrow = TRUE)))
  })


  # regions
  A <- st_multi_difference(l=polygons[c(1:6)])
  B <- st_multi_difference(l=polygons[c(2,1,3:6)])
  C <- st_multi_difference(l=polygons[c(3,1,2,4:6)])
  D <- st_multi_difference(l=polygons[c(4,1:3,5,6)])
  E <- st_multi_difference(l=polygons[c(5,1:4,6)])
  F <- st_multi_difference(l=polygons[c(6,1:5)])
  AB <- st_difference(st_intersection(polygons[[1]],polygons[[2]]),st_multi_union(l=polygons[c(-1,-2)]))
  AC <- st_difference(st_intersection(polygons[[1]],polygons[[3]]),st_multi_union(l=polygons[c(-1,-3)]))
  AD <- st_difference(st_intersection(polygons[[1]],polygons[[4]]),st_multi_union(l=polygons[c(-1,-4)]))
  AE <- st_difference(st_intersection(polygons[[1]],polygons[[5]]),st_multi_union(l=polygons[c(-1,-5)]))
  AF <- st_difference(st_intersection(polygons[[1]],polygons[[4]]),st_multi_union(l=polygons[3:6]))
  BC <- st_difference(st_intersection(polygons[[3]],polygons[[2]]),st_multi_union(l=polygons[3:6]))
  BD <- st_difference(st_intersection(polygons[[4]],polygons[[2]]),st_multi_union(l=polygons[3:6]))
  BE <- st_difference(st_intersection(polygons[[4]],polygons[[2]]),st_multi_union(l=polygons[3:6]))
  BF <- st_difference(st_intersection(polygons[[4]],polygons[[2]]),st_multi_union(l=polygons[3:6]))
  CD <- st_difference(st_intersection(polygons[[3]],polygons[[4]]),st_multi_union(l=polygons[3:6]))
  CE <- st_difference(st_intersection(polygons[[3]],polygons[[4]]),st_multi_union(l=polygons[3:6]))
  CF <- st_difference(st_intersection(polygons[[3]],polygons[[4]]),st_multi_union(l=polygons[3:6]))
  DE <- st_difference(st_intersection(polygons[[3]],polygons[[4]]),st_multi_union(l=polygons[3:6]))
  DF <- st_difference(st_intersection(polygons[[3]],polygons[[4]]),st_multi_union(l=polygons[3:6]))
  EF <- st_difference(st_intersection(polygons[[3]],polygons[[4]]),st_multi_union(l=polygons[3:6]))
  ABC <- st_difference(st_multi_intersection(l=polygons[1:3]),polygons[[4]])
  ABD <- st_difference(st_multi_intersection(l=polygons[c(1,2,4)]),polygons[[3]])
  ACD <- st_difference(st_multi_intersection(l=polygons[c(1,3,4)]),polygons[[2]])
  BCD <- st_difference(st_multi_intersection(l=polygons[c(4,2,3)]),polygons[[1]])
  AB
  ABCDEF <- st_multi_intersection(l=polygons)

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
