#' shapes: shape data used to setup Venn plot
#'
#' a collection of geometric shapes, which defined the edge and label of sets in a Venn plot.
#' use `plot_shapes()` to see some of them.
#'
#' @format a list with several slots
#'  see "?VennPlotData".
#'
#' @source
#' - The `venn` datasets authored by Adrian Dusa (<https://CRAN.R-project.org/package=venn>).
#'
#' - Parameters used to generate fancy four set ellipses are adopted from `VennDiagram`(<https://CRAN.R-project.org/package=VennDiagram>).
#'
#' - [Wiki](https://upload.wikimedia.org/wikipedia/commons/5/56/6-set_Venn_diagram_SMIL.svg)
#' @name shapes
#' @docType data
#' @md
NULL

#' plot all shapes provided by internal dataset
#'
#' These shapes are mainly collected from the package `venn`, and `VennDiagram`.
#' For Venn plot with more than 4 sets, it is usually impossible to plot with
#' simple circle or ellipse. So we need to use a predefined coordinates in plot.
#'
#' - Shape 101, 201, 301, 401, 402, 501, 502, 601 and 701 are from `venn`
#' - Shape 401f is from `VennDiagram`
#'
#' see `data-raw/shapes.R` to find how we incorporate these data.
#'
#'
#' @importFrom graphics par
#' @import ggplot2
#' @export
#' @examples
#' plot_shapes()
#' @md
plot_shapes <- function(){
  plots = lapply(shapes, plot_shape_edge)
  aplot::plot_list(gglist = plots, widths = 1)
}

#' Plot the set edge of a VennPlotData
#'
#' This is for viewing the shape id and appearance of the shape.
#'
#' @param x a VennPlotData object
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#'   shape = get_shape_by_id("301")
#'   plot_shape_edge(shape)
plot_shape_edge = function(x){
  if (!inherits(x, "VennPlotData"))
    stop("Try to plot a non-VennPlotData object. Please check.")
  id = get_shape_id(x)
  edge = get_shape_setedge(x)
  ggplot2::ggplot(edge, aes(.data$X, .data$Y, group = id)) +
    ggplot2::geom_path() +
    ggplot2::coord_equal() +
    ggplot2::labs(title = id) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}


