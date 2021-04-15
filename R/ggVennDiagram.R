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
ggVennDiagram <- function(x, category.names=names(x),
                          show_intersect = FALSE,
                          label=c("count","percent","both","none"),
                          label_alpha=0.5,
                          label_geom=c("label","text"),
                          lty=1, color="grey",...){

  if (!is.list(x)){
    stop(simpleError("ggVennDiagram() requires at least a list."))
  }
  names(x) <- category.names
  dimension <- length(x)
  label <- match.arg(label)
  label_geom <- match.arg(label_geom)
  if (dimension <= 6){
    plot_venn(x, show_intersect = show_intersect,label = label, label_alpha=label_alpha, label_geom = label_geom,lty=lty,color=color,...)
  }
  else{
    stop("Only support 2-6 dimension Venn diagram.")
  }
}




#' plot codes
#'
#' @inheritParams ggVennDiagram
#'
#' @import ggplot2
#'
#' @return ggplot object
plot_venn <- function(x, show_intersect, label, label_geom, label_alpha, lty, ...){
  venn <- Venn(x)
  data <- process_data(venn)
  p <- ggplot() +
    geom_sf(aes_string(fill="count"), data = data@region) +
    geom_sf(aes_string(color = "id"), size = 1, lty = lty, data = data@setEdge, show.legend = F) +
    geom_sf_text(aes_string(label = "name"), data = data@setLabel) +
    theme_void()

  if (label == "none"){
    return(p)
  }
  else{
    region_label <- data@region %>%
      dplyr::filter(component == "region") %>%
      dplyr::mutate(percent = paste(round(count*100/sum(count)),"%", sep="")) %>%
      dplyr::mutate(both = paste(count,percent,sep = "\n"))
    if (label_geom == "label"){
      p + geom_sf_label(aes_string(label=label), data = region_label, alpha=label_alpha, label.size = NA)
    } else if (label_geom == "text"){
      p + geom_sf_text(aes_string(label=label), data = region_label, alpha=label_alpha)
    } else {
      p
    }
  }
}



