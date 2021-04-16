## code to prepare `shapes` dataset goes here

# construct a sf object for ploting Venn
# columns in `shapes`
# - nsets: number of sets, 1:7
# - type: c("ellipse","triangle","polygon", "circle")
# - shape_id: shape id, each shape has three components, and belongs to different type
# - component: c("setEdge","setLabel")
# - id: id of set/region
# - xy: coordination of regions

# 4d ellipse
f4e <- build_shape(
  edge = fancy_4d_ellipse(),
  label = fancy_4d_ellipse_label(),
  shape_id = "401f",
  type = "ellipse"
)


# 3d circle
f3c <- build_shape(
  edge = fancy_3d_circle(),
  label = fancy_3d_circle_label(),
  shape_id = "301f",
  type = "circle"
)

# 2d circle
f2c <- build_shape(
  edge = fancy_2d_circle(),
  label = fancy_2d_circle_label(),
  shape_id = "201f",
  type = "circle"
)

# 6d triangle
f6t <- build_shape(
  edge = fancy_6d_triangle(),
  label = fancy_6d_triangle_label(),
  shape_id = "601f",
  type = "triangle"
)

# import venn:::sets dataset
transform_venn_data <- function(sets){
  sets <- sets %>%
    dplyr::filter(!is.na(x), !is.na(y)) %>%
    tidyr::nest(xy = c(x,y)) %>%
    dplyr::mutate(type = ifelse(v == 1, "ellipse", "polygon"),
           nsets = s,
           shape_id = paste(s,v+1,sep = "0"),
           id = n,
           xy = as.matrix(xy)) %>%
    dplyr::select(nsets, type, shape_id, id, xy)
  sets$xy <- lapply(sets$xy, as.matrix)
  sets
}
sets <- venn:::sets
sets <- transform_venn_data(sets) %>% dplyr::mutate(component = "setEdge")

# label position indicated by venn::venn()
scoords <- data.frame(
  s = c(1, rep(2, 2), rep(3, 3), rep(4, 4), rep(5, 10), rep(6, 6), rep(7, 7), rep(4, 4)),
  v = c(rep(0, 1 + 2 + 3), rep(1, 4), rep(0:1, each = 5), rep(0, 6 + 7), rep(0, 4)),
  x = c(500, 250, 750, 100, 500, 900,  88, 263, 713, 888,     80, 535, 900, 700, 120,      88, 533, 850, 750, 163,       100, 500, 910, 925, 550, 100, 220, 685, 935, 935, 600, 155,  50,  85, 220, 780, 915),
  y = c(780, 780, 780, 560, 910, 560, 663, 850, 850, 663,    800, 960, 700,  50, 120,     750, 963, 688,  40,  88,       860, 975, 775, 165,  30, 140, 955, 980, 780, 200,  15, 120, 690, 670, 850, 850, 670)
) %>%
  dplyr::group_by(s,v) %>%
  dplyr::mutate(n=dplyr::row_number()) %>%
  dplyr::ungroup()
scoords <- transform_venn_data(scoords) %>% dplyr::mutate(component = "setLabel")

shapes <- dplyr::bind_rows(f4e, f3c, f2c, f6t, sets, scoords)

# shapes <- NULL
usethis::use_data(shapes, overwrite = TRUE, internal = TRUE)
