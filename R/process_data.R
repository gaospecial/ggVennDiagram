setGeneric("process_data", function(venn) standardGeneric("process_data"))

#' get plot data
#' @param venn a Venn object
#' @export
setMethod("process_data", signature = c("Venn"),
          function(venn){
            shape <- get_shape_data(venn)
            plot_data <- VennPlotData(setEdge = filter(shape, component == "setEdge") %>% pull(xy),
                                      setLabel = filter(shape, component == "setLabel") %>% pull(xy))
            set_data <- process_setEdge_data(venn)
            label_data <- process_setLabel_data(venn)
            item_data <- process_item_data(venn)
            plot_data@setEdge <- left_join(plot_data@setEdge, set_data, by = "id")
            plot_data@setLabel <- left_join(plot_data@setLabel, label_data, by = "id")
            plot_data@region <- left_join(plot_data@region, item_data, by = "id")
            return(plot_data)
          })

get_shape_data <- function(venn){
  n = length(venn@sets)
  data <- shapes %>% filter(nsets == n)
  if (length(unique(data$shape_id))>1) {
    warnings("More than one shapes are available for ", n, " sets Venn plot. ",
             "Will choose one randomly.")
    id <- sample(unique(data$shape_id), 1)
    data <- data %>% filter(shape_id == id)
  }

  data
}

process_setEdge_data <- function(venn){
  tibble::tibble(
    component = "setEdge",
    id = as.character(seq_along(venn@sets)),
    count = sapply(venn@sets, length),
    name = venn@names
  )
}


process_setLabel_data <- function(venn){
  tibble::tibble(
    component = "setLabel",
    id = as.character(seq_along(venn@sets)),
    name = venn@names
  )
}

process_item_data <- function(venn){
  region_items <- get_region_items(venn)
  counts <- sapply(region_items, length)
  region_ids <- get_region_ids(venn)
  region_names <- get_region_names(venn)
  tibble::tibble(
    component = "region",
    id = region_ids,
    item = region_items,
    count = counts,
    name = region_names
  )
}



