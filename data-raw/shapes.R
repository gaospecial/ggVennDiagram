## code to prepare `shapes` dataset goes here

# construct a sf object for ploting Venn
# columns in `shapes`
# - shape_id: shape id, each shape has three components, and belongs to different type
# - nsets: number of sets, 1:7
# - component: c("setEdge","setLabel","region")
# - type: c("ellipses","triangle","polygon")
# - no: No. of shape
# - id: id of the regions of shape
# - geometry: coordination of regions
library(sf)

# 4d ellipses
parameters <- list(c(0.35, 0.47, 0.35, 0.20, 135),
                   c(0.50, 0.57, 0.35, 0.15, 135),
                   c(0.50, 0.57, 0.33, 0.15,  45),
                   c(0.65, 0.47, 0.35, 0.20,  45))
ellipses <- lapply(parameters,function(x){
  a <- VennDiagram::ell2poly(x[[1]],x[[2]],x[[3]],x[[4]],x[[5]],n.sides = 3000) %>%
    data.frame()
  rbind(a, a[1,])
})


label_position <- data.frame(x = c(0.08, 0.26, 0.71, 0.93),
                             y = c(0.78, 0.86, 0.85, 0.78))
points <- lapply(seq_len(nrow(label_position)),function(i){
  st_point(as.matrix(label_position[i,]))
})

category_labels <- tibble::tibble(
  component = "setLabel",
  id    = as.character(seq_len(length(ellipses))),
  geometry = points
) %>%
  st_as_sf()

linestrings <- lapply(ellipses, function(x) st_linestring(as.matrix(x)))
sets <- tibble::tibble(
  component = "setEdge",
  id = as.character(seq_len(length(ellipses))),
  geometry = linestrings
) %>%
  st_as_sf()

# region polygons
polygons <- lapply(ellipses,function(x)st_polygon(list(as.matrix(x))))
polygon <- Polygon(polygons)
regions <- get_region_items(polygon)
region_id <- get_region_ids(polygon)
regions <- tibble::tibble(
  component = "region",
  id = region_id,
  geometry = regions
) %>%
  st_as_sf()

shapes <- rbind(sets, regions, category_labels) %>%
  mutate(nsets = 4, type = "ellipse", shape_id = 1)

usethis::use_data(shapes, overwrite = TRUE, internal = TRUE)
