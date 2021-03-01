#' ggVennDiagram
#'
#' @param x list of items
#' @param n.sides set how many points been generated for one ellipse, the more points, the better resolution.
#' @param show_intersect whether add a hidden text to polygons in the plot, the text can be further visualized by `plotly::ggplotly()`
#' @param label select one from c("count","percent","both")
#' @param label_geom choose from geom_label and geom_text
#' @param label_alpha set 0 to remove label background
#' @param category.names default is names(x)
#' @param ... Other arguments passed on to the polygon layer.
#' @param lty line type of polygons
#' @param color line color of polygons
#'
#' @return A ggplot object
#' @export
#' @examples
#' library(ggVennDiagram)
#' x <- list(A=1:5,B=2:7,C=3:6,D=4:9)
#' ggVennDiagram(x)  # 4d venn
#' ggVennDiagram(x[1:3])  # 3d venn
#' ggVennDiagram(x[1:2])  # 2d venn
ggVennDiagram <- function(x, category.names=names(x),show_intersect = FALSE, n.sides=3000,label="both",label_alpha=0.5,label_geom=geom_label, lty=1,color="grey",...){
  dimension <- length(x)
  if (dimension == 6){
    draw_6d_venn(x, n.sides=n.sides,category.names=category.names,show_intersect = show_intersect,label = label, label_alpha=label_alpha, label_geom = label_geom,lty=lty,color=color,...)
  }
  else if (dimension == 4){
    draw_4d_venn(x, n.sides=n.sides,category.names=category.names,show_intersect = show_intersect, label = label, label_alpha=label_alpha, label_geom = label_geom, lty=lty,color=color,...)
  }
  else if (dimension == 3){
    draw_3d_venn(x, n.sides=n.sides,category.names=category.names,show_intersect = show_intersect,label = label, label_alpha=label_alpha, label_geom = label_geom,lty=lty,color=color,...)
  }
  else if (dimension == 2){
    draw_2d_venn(x, n.sides=n.sides,category.names=category.names,show_intersect = show_intersect,label = label, label_alpha=label_alpha, label_geom = label_geom,lty=lty,color=color,...)
  }
  else{
    stop("Only support 2-4 dimension venn diagram.")
  }
}

#' get a list of items in Venn regions
#' @inheritParams ggVennDiagram
#'
#' @return a list of region items
#' @export
get_region_items <- function(x, category.names=names(x)){
  if (!identical(category.names,names(x))){
    message("This function returns a list named by alphabet letters")
    message("The mapping between letters and categories is as follows:")
    message(paste(paste(names(x),category.names,sep = ": "), collapse = "\n"))
  }

  dimension <- length(x)
  if (dimension == 6){
    six_dimension_region_items(x)
  }
  else if (dimension == 4){
    four_dimension_region_items(x)
  }
  else if (dimension == 3){
    three_dimension_region_items(x)
  }
  else if (dimension == 2){
    two_dimension_region_items(x)
  }
  else{
    stop("Only support 2-4 dimension venn diagram.")
  }
}


#' plot codes
#'
#' @param region_data a list of two dataframes, which were used to plot polygon and label latter.
#' @param category name of Set
#' @param counts counts of items for every combinations
#' @inheritParams ggVennDiagram
#'
#' @import ggplot2
#'
#' @return ggplot object
plot_venn <- function(region_data, category, counts,show_intersect, label, label_geom, label_alpha, ...){
  polygon <- region_data[[1]]
  center <- region_data[[2]]
  if(show_intersect) {
    polygon_aes <- aes_string(fill="count",group="group",text="text")
  } else {
    polygon_aes <- aes_string(fill="count",group="group")
  }
  suppressWarnings(
    p <- ggplot() + aes_string("x","y") +
      geom_polygon(mapping = polygon_aes,data = merge(polygon,counts),...) +
      geom_text(aes(label=label),data=category,fontface="bold",color="black") +
      theme_void() + scale_fill_gradient(low="white",high = "red") +
      coord_fixed() +
      theme(legend.position = "right")
  )

  if (is.null(label)){
    return(p)
  }
  else{
    counts <- counts %>%
      mutate(percent=paste(round(.data$count*100/sum(.data$count),digits = 2),"%",sep="")) %>%
      mutate(label = paste(.data$count,"\n","(",.data$percent,")",sep=""))
    data <- merge(counts,center)
    if (label == "count"){
      p + label_geom(aes(label=count),data=data,label.size = NA, alpha=label_alpha)
    }
    else if (label == "percent"){
      p + label_geom(aes_string(label="percent"),data=data,label.size = NA, alpha=label_alpha)
    }
    else if (label == "both"){
      p + label_geom(aes_string(label="label"),data=data,label.size = NA,alpha=label_alpha)
    }
  }
}


#' generating a circle
#'
#' @param x,y center of circle
#' @param r radius of circle
#' @param n points (resolution)
#'
#' @return a data.frame representing circle position
circle <- function(x,y,r,n=1000){
  angles <- seq(0,2*pi,length.out = n)
  xv <- cos(angles) * r + x
  yv <- sin(angles) * r + y
  xv <- round(xv,6)
  yv <- round(yv,6)
  data.frame(x=xv,y=yv)
}
