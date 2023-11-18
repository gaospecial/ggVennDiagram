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
plot_shapes <- function(){
  plots = lapply(shapes, plot_shape_edge)
  aplot::plot_list(gglist = plots, widths = 1)
}

plot_shape_edge = function(x){
  id = get_shape_id(x)
  edge = get_shape_setedge(x)
  ggplot2::ggplot(edge) +
    ggplot2::geom_sf() +
    ggplot2::labs(title = id) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
}


