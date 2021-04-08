setGeneric("process_data", function(venn) standardGeneric("process_data"))

#' get plot data
#' @param venn a Venn object
#' @export
setMethod("process_data", signature = c("Venn"),
          function(venn){
            shape_data <- process_shape_data(venn)
            set_data <- process_setEdge_data(venn)
            item_data <- process_item_data(venn)
            label_data <- process_setLabel_data(venn)
            data <- shape_data %>%
              left_join(bind_rows(set_data,item_data,label_data),
                        by = c("component","id"))
            return(data)
          })

process_shape_data <- function(venn){
  n = length(venn@sets)
  data <- shapes %>% filter(nsets == n)
  if (length(unique(data$shape_id))>1) {
    warnings("More than one shapes are available for ", n, " sets Venn plot. ",
             "Will choose one randomly.")
    id <- sample(unique(data$shape_id), 1)
    data <- data %>% filter(shape_id == id)
  }
  return(data)
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



