#' plot all shapes provided by internal dataset
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
