test_that("multiplication works", {
  genes <- paste("gene",1:1000,sep="")
  set.seed(20210408)
  x <- list(A=sample(genes,300),B=sample(genes,525),C=sample(genes,440),D=sample(genes,350))
  venn <- Venn(x)

  region_items <- get_region_items(venn)
  counts <- sapply(region_items, length)
  region_ids <- get_region_ids(venn)
  items <- tibble::tibble(
    group = "region",
    id = region_ids,
    item = region_items,
    count = counts
  )
  set_label <- tibble::tibble(
    group = "label",
    id = as.character(seq_along(x)),
    setName = names(x)
  )
  data <- shapes %>% left_join(items) %>%
    left_join(set_label)
  p <- ggplot(data) +
    geom_sf(aes(fill=count), data = ~ filter(.x, group == "region")) +
    geom_sf(aes(color = id), size = 1,data = ~ filter(.x, group == "set"), show.legend = F) +
    geom_sf_text(aes(label = setName), fontface = "bold", data = function(d) filter(d, group == "label"), vjust = 0) +
    geom_sf_label(aes(label=count), data = function(d) filter(d, group == "region")) +
    theme_void()
  expect_is(p, "ggplot")
})
