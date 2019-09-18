#' ggVennDiagram
#'
#' @param x list of items
#' @param n.sides set how many points been generated for one ellipse, the more points, the better resolution.
#' @param label select one from c("count","percent","both")
#' @param category.names default is names(x)
#' @param ... Other arguments passed on to the polygon layer.
#'
#' @return A ggplot object
#'
#' @export
#'
#' @examples
#' x <- list(A=1:5,B=2:7,C=3:6,D=4:9)
#' ggVennDiagram(x)  # 4d venn
#' ggVennDiagram(x[1:3])  # 3d venn
#' ggVennDiagram(x[1:2])  # 2d venn
ggVennDiagram <- function(x, category.names=names(x), n.sides=3000,label="both",lty=1,color="grey",...){
  dimension <- length(x)
  if (dimension == 4){
    draw_4d_venn(x, n.sides=n.sides,category.names=category.names,label = label,lty=lty,color=color,...)
  }
  else if (dimension == 3){
    draw_3d_venn(x, n.sides=n.sides,category.names=category.names,label = label,lty=lty,color=color,...)
  }
  else if (dimension == 2){
    draw_2d_venn(x, n.sides=n.sides,category.names=category.names,label = label,lty=lty,color=color,...)
  }
  else{
    stop("Only support 2-4 dimension venn diagram.")
  }
}


#' reuse plot codes
#'
#' @param region_data a list of two dataframes, which were used to plot polygon and label latter.
#' @param category name of Set
#' @param counts counts of items for every combinations
#' @inheritParams ggVennDiagram
#'
#' @import ggplot2
#'
#' @return ggplot object
plot_venn <- function(region_data, category, counts, label, ...){
  polygon <- region_data[[1]]
  center <- region_data[[2]]
  p <- ggplot() + aes_string("x","y") +
    geom_text(aes(label=label),data=category,fontface="bold",color="black") +
    geom_polygon(aes(fill=count,group=group),data = merge(polygon,counts),...) +
    theme_void() + scale_fill_gradient(low="white",high = "red") +
    coord_fixed() +
    theme(legend.position = "right")
  if (is.null(label)){
    return(p)
  }
  else{
    counts %<>% mutate(percent=paste(round(count*100/sum(count),digits = 2),"%",sep="")) %>%
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


