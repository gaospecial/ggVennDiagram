#' draw 5d venn diagram
#'
#' @rdname draw_venn
#'
#'
draw_5d_venn <- function(x, n.sides, category.names, label,...){

  category <- data.frame(x = c(0.13, 0.26, 0.71, 0.86, 1),
                         y = c(0.77, 0.86, 0.85, 0.77, 1),
                         label = category.names)

  region_data <- five_dimension_ellipse_regions(n.sides)


  counts <- five_dimension_region_values(x)

  plot_venn(region_data,category, counts, label, ...)


}

#' calculating count values
#'
#' @param x a list of vector items.
#'
five_dimension_region_values <- function(x){

  if (length(x)!=5) stop("5D venn needs a list of five vectors.")

  A <- multi_setdiff(l=x[c(1,3,3,4,5)])
  B <- multi_setdiff(l=x[c(2,1,3,4,5)])
  C <- multi_setdiff(l=x[c(3,1,2,4,5)])
  D <- multi_setdiff(l=x[c(4,1,2,3,5)])
  E <- multi_setdiff(l=x[c(5,1,2,3,4)])
  AB <- setdiff(intersect(x[[1]],x[[2]]),multi_union(l=x[c(3,4,5)]))
  AC <- setdiff(intersect(x[[1]],x[[3]]),multi_union(l=x[c(2,4,5)]))
  AD <- setdiff(intersect(x[[1]],x[[4]]),multi_union(l=x[c(2,3,5)]))
  AE <- setdiff(intersect(x[[1]],x[[5]]),multi_union(l=x[c(2,3,4)]))
  BC <- setdiff(intersect(x[[2]],x[[3]]),multi_union(l=x[c(1,4,5)]))
  BD <- setdiff(intersect(x[[2]],x[[4]]),multi_union(l=x[c(1,3,5)]))
  BE <- setdiff(intersect(x[[1]],x[[5]]),multi_union(l=x[c(2,3,4)]))
  CD <- setdiff(intersect(x[[3]],x[[4]]),multi_union(l=x[c(1,2,5)]))
  CE <- setdiff(intersect(x[[3]],x[[5]]),multi_union(l=x[c(1,2,4)]))
  DE <- setdiff(intersect(x[[4]],x[[5]]),multi_union(l=x[c(1,2,3)]))
  ABC <- setdiff(multi_intersect(l=x[c(1,2,3)]),union(x[[4]],x[[5]]))
  ABD <- setdiff(multi_intersect(l=x[c(1,2,4)]),union(x[[3]],x[[5]]))
  ABE <- setdiff(multi_intersect(l=x[c(1,2,5)]),union(x[[3]],x[[4]]))
  ACD <- setdiff(multi_intersect(l=x[c(1,3,4)]),union(x[[2]],x[[5]]))
  ACE <- setdiff(multi_intersect(l=x[c(1,3,5)]),union(x[[2]],x[[4]]))
  ADE <- setdiff(multi_intersect(l=x[c(1,4,5)]),union(x[[2]],x[[3]]))
  BCD <- setdiff(multi_intersect(l=x[c(2,3,4)]),union(x[[1]],x[[5]]))
  BCE <- setdiff(multi_intersect(l=x[c(2,3,5)]),union(x[[1]],x[[4]]))
  BDE <- setdiff(multi_intersect(l=x[c(2,4,5)]),union(x[[1]],x[[3]]))
  CDE <- setdiff(multi_intersect(l=x[c(3,4,5)]),union(x[[1]],x[[2]]))
  ABCD <- setdiff(multi_intersect(l=x[c(1,2,3,4)]),x[5])
  ABCE <- setdiff(multi_intersect(l=x[c(1,2,3,5)]),x[4])
  ABDE <- setdiff(multi_intersect(l=x[c(1,2,4,5)]),x[3])
  ACDE <- setdiff(multi_intersect(l=x[c(1,3,4,5)]),x[2])
  BCDE <- setdiff(multi_intersect(l=x[c(2,3,4,5)]),x[1])
  ABCDE <- multi_intersect(l=x[c(1,2,3,4,5)])

  str <- c("A,B,C,D,E,AB,AC,AD,AE,BC,BD,BE,CD,CE,DE,ABC,ABD,ABE,ACD,ACE,ADE,BCD,BCE,CDE,ABCD,ABCE,ABDE,ACDE,BCDE,ABCDE")
  items <- eval(parse(text=paste0("list(",str,")",collapse = "")))
  names(items) <- unlist(strsplit(str,","))
  values <- sapply(items, length)
  data.frame(group=names(items),count=values,stringsAsFactors = F)
}

#' coordinations of polygon regions/centers for 4d venn diagram
#'
#' @importFrom VennDiagram ell2poly
#' @importFrom sf st_polygon st_difference st_intersection st_centroid st_union
#' @import dplyr
five_dimension_ellipse_regions <- function(n.sides){

  # ellipse
  parameters <- list(c(9,14, 40,80,72),
                     c(7,16, 42,78,144),
                     c(5,12, 46,70,216),
                     c(5,14, 37,64,288),
                     c(5,12, 40,65,360))
  ellipses <- lapply(parameters,function(x){
    do.call(ell2poly,as.list(c(x,n.sides))) %>%
      data.frame() %>%
      mutate(x=round(x,6),y=round(y,6))
  })

  polygons <- lapply(ellipses,function(x)st_polygon(list(as.matrix(x))))

  # regions
  A <- st_multi_difference(l=polygons[c(1,3,3,4,5)])
  B <- st_multi_difference(l=polygons[c(2,1,3,4,5)])
  C <- st_multi_difference(l=polygons[c(3,1,2,4,5)])
  D <- st_multi_difference(l=polygons[c(4,1,2,3,5)])
  E <- st_multi_difference(l=polygons[c(5,1,2,3,4)])
  AB <- st_difference(st_intersection(polygons[[1]],polygons[[2]]),st_multi_union(l=polygons[c(3,4,5)]))
  AC <- st_difference(st_intersection(polygons[[1]],polygons[[3]]),st_multi_union(l=polygons[c(2,4,5)]))
  AD <- st_difference(st_intersection(polygons[[1]],polygons[[4]]),st_multi_union(l=polygons[c(2,3,5)]))
  AE <- st_difference(st_intersection(polygons[[1]],polygons[[5]]),st_multi_union(l=polygons[c(2,3,4)]))
  BC <- st_difference(st_intersection(polygons[[2]],polygons[[3]]),st_multi_union(l=polygons[c(1,4,5)]))
  BD <- st_difference(st_intersection(polygons[[2]],polygons[[4]]),st_multi_union(l=polygons[c(1,3,5)]))
  BE <- st_difference(st_intersection(polygons[[1]],polygons[[5]]),st_multi_union(l=polygons[c(2,3,4)]))
  CD <- st_difference(st_intersection(polygons[[3]],polygons[[4]]),st_multi_union(l=polygons[c(1,2,5)]))
  CE <- st_difference(st_intersection(polygons[[3]],polygons[[5]]),st_multi_union(l=polygons[c(1,2,4)]))
  DE <- st_difference(st_intersection(polygons[[4]],polygons[[5]]),st_multi_union(l=polygons[c(1,2,3)]))
  ABC <- st_difference(st_multi_intersection(l=polygons[c(1,2,3)]),st_union(polygons[[4]],polygons[[5]]))
  ABD <- st_difference(st_multi_intersection(l=polygons[c(1,2,4)]),st_union(polygons[[3]],polygons[[5]]))
  ABE <- st_difference(st_multi_intersection(l=polygons[c(1,2,5)]),st_union(polygons[[3]],polygons[[4]]))
  ACD <- st_difference(st_multi_intersection(l=polygons[c(1,3,4)]),st_union(polygons[[2]],polygons[[5]]))
  ACE <- st_difference(st_multi_intersection(l=polygons[c(1,3,5)]),st_union(polygons[[2]],polygons[[4]]))
  ADE <- st_difference(st_multi_intersection(l=polygons[c(1,4,5)]),st_union(polygons[[2]],polygons[[3]]))
  BCD <- st_difference(st_multi_intersection(l=polygons[c(2,3,4)]),st_union(polygons[[1]],polygons[[5]]))
  BCE <- st_difference(st_multi_intersection(l=polygons[c(2,3,5)]),st_union(polygons[[1]],polygons[[4]]))
  BDE <- st_difference(st_multi_intersection(l=polygons[c(2,4,5)]),st_union(polygons[[1]],polygons[[3]]))
  CDE <- st_difference(st_multi_intersection(l=polygons[c(3,4,5)]),st_union(polygons[[1]],polygons[[2]]))
  ABCD <- st_difference(st_multi_intersection(l=polygons[c(1,2,3,4)]),polygons[[5]])
  ABCE <- st_difference(st_multi_intersection(l=polygons[c(1,2,3,5)]),polygons[[4]])
  ABDE <- st_difference(st_multi_intersection(l=polygons[c(1,2,4,5)]),polygons[[3]])
  ACDE <- st_difference(st_multi_intersection(l=polygons[c(1,3,4,5)]),polygons[[2]])
  BCDE <- st_difference(st_multi_intersection(l=polygons[c(2,3,4,5)]),polygons[[1]])
  ABCDE <- st_multi_intersection(l=polygons[c(1,2,3,4,5)])

  str <- c("A,B,C,D,E,AB,AC,AD,AE,BC,BD,BE,CD,CE,DE,ABC,ABD,ABE,ACD,ACE,ADE,BCD,BCE,CDE,ABCD,ABCE,ABDE,ACDE,BCDE,ABCDE")
  polygon_name <- unlist(strsplit(str,","))
  polygon_list <- eval(parse(text=paste0("list(",str,")",collapse = "")))
  polygon_dfs <- lapply(1:length(polygon_list), function(i){
    df <- unlist(polygon_list[[i]])
    if (is.null(df)) return(NULL)
    df %<>% matrix(ncol = 2) %>% data.frame()
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
