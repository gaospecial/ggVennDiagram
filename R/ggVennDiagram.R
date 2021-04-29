#' `ggVennDiagram`: an easy to use Venn diagram generator
#'
#' Venn diagram is frequently used in scientific studies of many fields.
#' This package incorporates state-of-art Venn plot tools and provides a set of
#' easy-to-use functions to plot Venn. By dealing with a user-provided list,
#' which contains the sets of Venn, `ggVennDiagram` returns a structured data that
#' can be used to plot Venn. The data contains three slots: 1) the edge of Venn sets;
#' 2) the separated regions of Venn sets; 3) the labels of Venn sets.
#' By help from the package `venn`, it is possible to draw Venn diagram up to 7 sets.
#'
#' @docType package
#' @name ggVennDiagram-package
NULL

#' shapes: internal shape data
#'
#' a collection of geometric shapes, which defined the edge and label of sets in a Venn plot.
#' use `plot_shapes()` to see some of them.
#'
#' @format a tibble with 6 columns
#'
#'  * `nsets`: number of sets, from 1-7.
#'  * `type`: ellipse, circle or triangle
#'  - `shape_id`: to separate different shapes
#'  - `component`: each shape has two components, 'setEdge' and 'setLabel'
#'  - `id`: to separate edges/labels of a shape. For example, 4 sets shape will have ids of 1-4.
#'  - `xy`: coordinates
#' @md
#'
#' @source
#' - `venn:::sets`
#' - `library(VennDiagram)`
#' - [Wiki](https://upload.wikimedia.org/wikipedia/commons/5/56/6-set_Venn_diagram_SMIL.svg)
#' @md
#' @name vennplot-shapes
"shapes"

#' ggVennDiagram main parser
#'
#' @param x list of items
#' @param show_intersect if TRUE the text can be visualized by `plotly`
#' @param label select one from c("count","percent","both","none")
#' @param label_geom choose from c("label", "text")
#' @param label_alpha set 0 to remove label background
#' @param category.names default is names(x)
#' @param ... Other arguments passed on to downstream functions.
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
                          ...){

  if (!is.list(x)){
    stop(simpleError("ggVennDiagram() requires at least a list."))
  }
  names(x) <- category.names
  dimension <- length(x)
  label <- match.arg(label)
  label_geom <- match.arg(label_geom)
  if (dimension <= 7){
    plot_venn(x, show_intersect = show_intersect,label = label, label_alpha=label_alpha, label_geom = label_geom)
  }
  else{
    stop("Only support 2-7 dimension Venn diagram.")
  }
}




#' plot codes
#'
#' @inheritParams ggVennDiagram
#' @param percent_digit number of digits when formatting percent label (0)
#' @param txtWidth width of text used in showing intersect members (40)
#'
#' @import ggplot2
#'
#' @return ggplot object, or plotly object if show_intersect is TRUE
plot_venn <- function(x,
                      show_intersect = FALSE,
                      label = "count",
                      label_geom = "label",
                      label_alpha = 0.3,
                      percent_digit = 0,
                      txtWidth = 40){
  venn <- Venn(x)
  data <- process_data(venn)
  p <- ggplot() +
    geom_sf(aes_string(fill="count"), data = data@region) +
    geom_sf(aes_string(color = "id"), size = 1, data = data@setEdge, show.legend = F) +
    geom_sf_text(aes_string(label = "name"), data = data@setLabel) +
    theme_void()

  if (label != "none" & show_intersect == FALSE){
    region_label <- data@region %>%
      dplyr::filter(.data$component == "region") %>%
      dplyr::mutate(percent = paste(round(.data$count*100/sum(.data$count), digits = percent_digit),"%", sep="")) %>%
      dplyr::mutate(both = paste(.data$count,.data$percent,sep = "\n"))
    if (label_geom == "label"){
      p <- p + geom_sf_label(aes_string(label=label), data = region_label, alpha=label_alpha, label.size = NA)
    }
    if (label_geom == "text"){
      p <- p + geom_sf_text(aes_string(label=label), data = region_label, alpha=label_alpha)
    }
  }

  if (show_intersect == TRUE){
    items <- data@region %>%
      dplyr::rowwise() %>%
      dplyr::mutate(text = stringr::str_wrap(paste0(.data$item, collapse = " "), width = txtWidth)) %>%
      sf::st_as_sf()
    p <- ggplot(items, aes_string(fill="count", text = 'text')) + geom_sf() +
      geom_sf_text(aes_string(label = "name"), data = data@setLabel, inherit.aes = F) +
      theme_void()
    ax <- list(
      showline = FALSE
    )
    p <- plotly::ggplotly(p) %>%
      plotly::style(hoveron = "fills+points") %>%
      plotly::layout(xaxis = ax, yaxis = ax)
  }

  p
}



