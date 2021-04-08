#' ggVennDiagram
#'
#' @param x list of items
#' @param show_intersect whether add a hidden text to polygons in the plot, the text can be further visualized by `plotly::ggplotly()`
#' @param label select one from c("count","percent","both"), set to NULL if you don't want label.
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
ggVennDiagram <- function(x, category.names=names(x),show_intersect = FALSE, label="count",label_alpha=0.5,label_geom=geom_label, lty=1,color="grey",...){
  dimension <- length(x)
  names(x) <- category.names
  venn <- Venn(sets = x)
  if (dimension <= 6){
    plot_venn(venn, show_intersect = show_intersect,label = label, label_alpha=label_alpha, label_geom = label_geom,lty=lty,color=color,...)
  }
  else{
    stop("Only support 2-4 dimension Venn diagram.")
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
plot_venn <- function(venn, category, counts,show_intersect, label, label_geom, label_alpha, ...){
  data <- process_data(venn)
  p <- ggplot(data) +
    geom_sf(aes(fill=count), data = ~ filter(.x, component == "region")) +
    geom_sf(aes(color = id), size = 1, data = ~ filter(.x, component == "setEdge"), show.legend = F) +
    geom_sf_text(aes(label = name), data = ~ filter(.x, component == "setLabel")) +
    theme_void()

  if (is.null(label)){
    return(p)
  }
  else{
    region_label <- p$data %>%
      filter(component == "region") %>%
      mutate(percent = paste(round(count*100/sum(count)),"%", sep="")) %>%
      mutate(both = paste(count,percent,sep = "\n"))
    if (label == "count"){
      p + geom_sf_label(aes(label=count), data = region_label, alpha=label_alpha, label.size = NA)
    }
    else if (label == "percent"){
      p + geom_sf_label(aes(label=percent), data = region_label, alpha=label_alpha, label.size = NA)
    }
    else if (label == "both"){
      p + geom_sf_label(aes(label=both), data = region_label, alpha=label_alpha, label.size = NA)
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
