## code to prepare `shapes` dataset goes here

rm(list = ls())
if (!requireNamespace("shapeMageR", quietly = TRUE)){
  devtools::load_all("../shapeMageR/")
}

## code to prepare `ggVennDiagramShapes` dataset goes here

## code to prepare `shapes` dataset goes here

# construct a sf object for plotting Venn Diagram
# columns in `shapes`
# - shapeId: shape id, each shape has three components, and belongs to different type
# - type: c("ellipse","triangle","polygon", "circle")
# - nsets: number of sets, 1:7
# - component: c("setEdge","setLabel")
#   - id: id of set/region
#   - geometry: coordination of regions


# 4d ellipse
f4e = VennPlotData(
  shapeId = "401f",
  type = "ellipse",
  setEdge = fancy_4d_ellipse(),  # how to store object in data.frame
  setLabel =  fancy_4d_ellipse_label())


# 3d circle
f3c = VennPlotData(
  setEdge = fancy_3d_circle(),
  setLabel =  fancy_3d_circle_label(),
  shapeId = "301f",
  type = "circle")

# 2d circle
f2c = VennPlotData(
  setEdge = fancy_2d_circle(),
  setLabel =  fancy_2d_circle_label(),
  shapeId = "201f",
  type = "circle")

# 6d triangle
f6t = VennPlotData(
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

# label position indicated by venn::venn()
scoords = data.frame(
  nsets = c(1,rep(2,2),rep(3,3),rep(4,4),rep(5,10),rep(6,6),rep(7,7),rep(4,4)),
  shapeId = c(rep(0, 1 + 2 + 3),rep(1, 4),rep(0:1, each = 5), rep(0, 6 + 7), rep(0, 4)),
  x = c(500,250,750,100,500,900,88,263,713,888,80,535,900,700,120,88,533,850,
        750,163,100,500,910,925,550,100,220,685,935,935,600,155,50,85,220,780,915),
  y = c(780,780,780,560,910,560,663,850,850,663,800,960,700,50,120,750,963,688,
        40,88,860,975,775,165,30,140,955,980,780,200,15,120,690,670,850,850,670)
) %>%
  dplyr::mutate(shapeId = paste(nsets, shapeId + 1, sep = "0"),
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
  label =  scoords$geometry[[which(scoords$shapeId == id)]][["xy"]]
  VennPlotData(setEdge = edge, setLabel = label, shapeId = id, type = type)
})

shapes =
  c(list(f4e), list(f3c), list(f2c), list(f6t), venn_shapes)
shapes = lapply(shapes, unclass)
sortby = sapply(shapes, get_shape_nsets) |> order()
shapes = shapes[sortby]
usethis::use_data(shapes, overwrite = TRUE, internal = TRUE)
