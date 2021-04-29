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
  edge <- shapes %>%
    dplyr::filter(.data$component == "setEdge", .data$type %in% c("ellipse","circle","polygon"))
  edge$xy <- lapply(edge$xy, dplyr::as_tibble)
  edge %>% tidyr::unnest(.data$xy) %>%
    dplyr::mutate_at("id",as.character) %>%
    dplyr::rowwise() %>%
    ggplot(aes_string(x="x",y="y",fill="id",group="id",color="id")) +
    geom_path(size=1,alpha=0.5,show.legend = F) +
    facet_wrap(~shape_id, scales = "free") +
    theme_void()
}
