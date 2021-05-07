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
#'
#' @import ggplot2
#' @export
#' @examples
#' plot_shapes()
plot_shapes <- function(){
  ids <- sort(unique(shapes$shape_id))
  ids <- ids[ids != "601f"]
  # windows()
  par(mfrow = c(3,4), mar = c(1,1,1,1))
  success <- lapply(seq_along(ids), function(i){
    id = ids[[i]]
    shapes %>% dplyr::filter(.data$component == "setEdge", .data$shape_id == {{id}}) %>%
      dplyr::pull(.data$xy) %>%
      sf::st_polygon() %>%
      plot(main = {{id}})
  })
}
