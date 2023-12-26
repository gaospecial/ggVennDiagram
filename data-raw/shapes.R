## code to prepare `shapes` dataset goes here

rm(list = ls())
devtools::install_github("gaospecial/shapeMageR")
library(shapeMageR)

## code to prepare `ggVennDiagramShapes` dataset goes here

# construct a object for plotting Venn Diagram
# columns in `shapes`
# - shapeId: shape id, each shape has three components, and belongs to different type
# - type: c("ellipse","triangle","polygon", "circle")
# - nsets: number of sets, 1:7
# - component: c("setEdge","setLabel","regionEdge","regionLabel")
#   - id: id of set/region
#   - x, y: coordination of edges/labels


# 4d ellipse
f4e = VennShape(
  shapeId = "401f",
  type = "ellipse",
  setEdge = fancy_4d_ellipse(),  # how to store object in data.frame
  setLabel =  fancy_4d_ellipse_label())


# 3d circle
f3c = VennShape(
  setEdge = fancy_3d_circle(),
  setLabel =  fancy_3d_circle_label(),
  shapeId = "301f",
  type = "circle")

# 2d circle
f2c = VennShape(
  setEdge = fancy_2d_circle(),
  setLabel =  fancy_2d_circle_label(),
  shapeId = "201f",
  type = "circle")

# 6d triangle
f6t = VennShape(
  setEdge = fancy_6d_triangle(),
  setLabel =  fancy_6d_triangle_label(),
  shapeId = "601t",
  type = "triangle")

####### Deal with venn datasets ########

# import venn:::sets dataset
sets = venn:::sets |>
  dplyr::filter(!is.na(x), !is.na(y))
colnames(sets) = c("nsets", "shapeId", "id", "x", "y")
sets = sets |>
  dplyr::mutate(shapeId = paste(nsets, shapeId + 1, sep = "0"))

# label position indicated by venn:::scoords
scoords = venn:::scoords
colnames(scoords) = c("nsets", "shapeId", "x", "y")
scoords = scoords |> dplyr::mutate(shapeId = paste(nsets, shapeId + 1, sep = "0"),
                id = dplyr::row_number(),
                .by = c(nsets, shapeId))

sets = sets |>   # nest two times
  tidyr::nest(xy = c(x, y), .by = nsets:id) |>
  tidyr::nest(geometry = c(xy), .by = nsets:shapeId)
sets$type = c(rep("circle", times = 3),
              "ellipse",
              rep("polygon", times = 5))
scoords = scoords |>
  tidyr::nest(xy = c(x, y), .by = nsets:id) |>
  tidyr::nest(geometry = c(xy), .by = nsets:shapeId)

if(nrow(sets) != nrow(scoords)){
  stop("Set edges and labels are not paired. Please check!")
}

venn_shapes = lapply(seq_len(nrow(sets)), function(i){
  id = sets$shapeId[[i]]
  type = sets$type[[i]]
  edge = sets$geometry[[i]][["xy"]]
  edge = lapply(edge, as.matrix)
  label =  scoords$geometry[[which(scoords$shapeId == id)]][["xy"]]
  label = lapply(label, as.matrix)
  VennShape(setEdge = edge, setLabel = label, shapeId = id, type = type)
})

shapes =
  c(list(f4e), list(f3c), list(f2c), list(f6t), venn_shapes)
shapes = lapply(shapes, function(x){
  tryCatch(unclass(x), error = function(e) NULL)
})
shapes = shapes[!sapply(shapes, is.null)]
sortby = sapply(shapes, get_shape_nsets) |> order()
shapes = shapes[sortby]
shapes = lapply(shapes, function(x){
  class(x) = "VennPlotData"
  return(x)
})
usethis::use_data(shapes, overwrite = TRUE, internal = TRUE)
