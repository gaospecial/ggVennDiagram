#' ggVennDiagram
#'
#' @param x list of items
#' @param n.sides set how many points been generated for one ellipse, the more points, the better resolution.
#' @param label select one from c("count","percent","both")
#' @param ... Other arguments passed on to the polygon layer.
#'
#' @return A ggplot object
#'
#' @examples
ggVennDiagram <- function(x, n.sides=3000,label=NULL,...){
  dimension <- length(x)
  if (dimension ==5){
    draw_5d_venn(x, n.sides=n.sides,category.names=names(x),label = label,...)
  }
  else if (dimension == 4){
    draw_4d_venn(x, n.sides=n.sides,category.names=names(x),label = label,...)
  }
  else if (dimension == 3){
    draw_3d_venn(x, n.sides=n.sides,category.names=names(x),label = label,...)
  }
  else if (dimension == 2){
    draw_2d_venn(x, n.sides=n.sides,category.names=names(x),label = label,...)
  }
  else{
    stop("Only support 2-5 dimension venn diagram.")
  }
}

#' draw 4d venn diagram
#' @param x
#' @param n.sides
#' @param category.names
#' @param label
#'
#' @importFrom ggtree theme_tree
#' @import ggplot2
#' @import dplyr
#'
draw_4d_venn <- function(x, n.sides, category.names, label,...){

  category <- data.frame(x = c(0.13, 0.26, 0.71, 0.86),
                         y = c(0.77, 0.86, 0.85, 0.77),
                         label = category.names)

  region_data <- four_dimension_ellipse_regions(n.sides)
  polygon <- region_data[[1]]
  center <- region_data[[2]]

  counts <- four_dimension_region_values(x)


  p <- ggplot() + aes_string("x","y") +
    geom_text(aes(label=label),data=category,fontface="bold",color="black") +
    geom_polygon(aes(fill=count,group=group),data = merge(polygon,counts),...) +
    theme_tree() +
    theme(legend.position = "right")
  if (is.null(label)){
    return(p)
  }
  else{
    counts %<>% mutate(percent=paste(round(count/sum(count),digits = 2),"%",sep="")) %>%
      mutate(label = paste(count,"\n","(",percent,")",sep=""))
    data <- merge(counts,center)
    if (label == "count"){
      p + geom_label(aes(label=count),data=data,label.size = NA, alpha=0.5)
    }
    else if (label == "percent"){
      p + geom_label(aes(label=percent),data=data,label.size = NA, alpha=0.5)
    }
    else if (label == "both"){
      p + geom_label(aes(label=label),data=data,label.size = NA,alpha=0.5)
    }
  }
}


#' calculating count values
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
four_dimension_region_values <- function(x){

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

  items <- list(A=A,B=B,C=C,D=D,AB=AB,AC=AC,AD=AD,BC=BC,BD=BD,CD=CD,ABC=ABC,ABD=ABD,ACD=ACD,BCD=BCD,ABCD=ABCD)

  values <- sapply(items, length)
  data.frame(group=names(items),count=values,stringsAsFactors = F)
}

#' coordinations of polygon regions/centers for 4d venn diagram
#'
#' @param n.sides
#'
#' @return
#' @export
#'
#' @importFrom VennDiagram ell2poly
#' @importFrom sf st_polygon st_difference st_intersection st_centroid st_union
#' @import dplyr
#' @examples
four_dimension_ellipse_regions <- function(n.sides){

  # ellipse
  parameters <- list(c(0.35, 0.47, 0.35, 0.20, 135),
                     c(0.50, 0.57, 0.35, 0.15, 135),
                     c(0.50, 0.57, 0.33, 0.15,  45),
                     c(0.65, 0.47, 0.35, 0.20,  45))
  ellipses <- lapply(parameters,function(x){
    do.call(ell2poly,as.list(c(x,n.sides))) %>%
      data.frame() %>%
      mutate(x=round(x,6),y=round(y,6))
  })

  polygons <- lapply(ellipses,function(x)st_polygon(list(as.matrix(x))))

  # regions
  A <- st_difference(st_difference(st_difference(polygons[[1]],polygons[[2]]),polygons[[3]]),polygons[[4]])
  B <- st_difference(st_difference(st_difference(polygons[[2]],polygons[[1]]),polygons[[3]]),polygons[[4]])
  C <- st_difference(st_difference(st_difference(polygons[[3]],polygons[[1]]),polygons[[2]]),polygons[[4]])
  D <- st_difference(st_difference(st_difference(polygons[[4]],polygons[[1]]),polygons[[3]]),polygons[[2]])
  AB <- st_difference(st_intersection(polygons[[1]],polygons[[2]]),st_union(polygons[[3]],polygons[[4]]))
  AC <- st_difference(st_intersection(polygons[[1]],polygons[[3]]),st_union(polygons[[2]],polygons[[4]]))
  AD <- st_difference(st_intersection(polygons[[1]],polygons[[4]]),st_union(polygons[[3]],polygons[[2]]))
  BC <- st_difference(st_intersection(polygons[[3]],polygons[[2]]),st_union(polygons[[1]],polygons[[4]]))
  BD <- st_difference(st_intersection(polygons[[4]],polygons[[2]]),st_union(polygons[[3]],polygons[[1]]))
  CD <- st_difference(st_intersection(polygons[[3]],polygons[[4]]),st_union(polygons[[1]],polygons[[2]]))
  ABC <- st_difference(st_intersection(st_intersection(polygons[[1]],polygons[[2]]),polygons[[3]]),polygons[[4]])
  ABD <- st_difference(st_intersection(st_intersection(polygons[[1]],polygons[[2]]),polygons[[4]]),polygons[[3]])
  ACD <- st_difference(st_intersection(st_intersection(polygons[[1]],polygons[[4]]),polygons[[3]]),polygons[[2]])
  BCD <- st_difference(st_intersection(st_intersection(polygons[[4]],polygons[[2]]),polygons[[3]]),polygons[[1]])
  ABCD <- st_intersection(st_intersection(st_intersection(polygons[[1]],polygons[[2]]),polygons[[3]]),polygons[[4]])

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
