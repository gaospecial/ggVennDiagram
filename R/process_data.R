setGeneric("process_data", function(venn) standardGeneric("process_data"))

#' get plot data
#' @param venn a Venn object
#' @export
setMethod("process_data", signature = c("Venn"),
          function(venn){
            shape_data <- process_shape_data(venn)
            item_data <- process_item_data(venn)
            label_data <- process_label_data(venn)
            data <- shape_data %>%
              left_join(item_data, by = c("component", "id")) %>%
              left_join(label_data, by = c("component", "id"))
            return(data)
          })

process_shape_data <- function(venn){
  n = length(venn@sets)
  shapes %>% filter(nsets == n)
}

process_item_data <- function(venn){
  region_items <- get_region_items(venn)
  counts <- sapply(region_items, length)
  region_ids <- get_region_ids(venn)
  tibble::tibble(
    component = "region",
    id = region_ids,
    item = region_items,
    count = counts
  )
}

process_label_data <- function(venn){
  tibble::tibble(
    component = "label",
    id = as.character(seq_along(venn@sets)),
    setName = venn@names
  )
}


