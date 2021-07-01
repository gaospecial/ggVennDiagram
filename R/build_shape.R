#' Helper function to add shape
#'
#' @param edge a list of xy matrix
#' @param label a list of xy matrix
#' @param nsets 2:7
#' @param type c("ellipse","triangle","polygon","circle")
#' @param shape_id a unique id
#'
#' @return a tibble with columns: nsets, type, shape_id, component, id, xy.
#'
#' @export
build_shape <- function(edge, label,
                      nsets = length(edge),
                      shape_id,
                      type = c("ellipse","triangle","polygon","circle")){

  if (sum(sapply(edge, is.matrix) == FALSE) >= 1)
    stop("The element in edge must be a two-column matrix.")
  if (sum(sapply(label, is.matrix) == FALSE) >= 1)
    stop("The element in edge must be a two-column matrix.")
  if (length(edge) != length(label))
    stop("Length of edge/label must be equal.")
  if (!is.list(edge) | !is.list(edge))
    stop("edge/label must be a list.")
  type <- match.arg(type)

  shape_edge <- tibble::tibble(
    nsets = nsets,
    type = type,
    shape_id = shape_id,
    component = "setEdge",
    id = seq_len(nsets),
    xy = edge
  )

  shape_label <- tibble::tibble(
    nsets = nsets,
    type = type,
    shape_id = shape_id,
    component = "setLabel",
    id = seq_len(nsets),
    xy = label
  )

  rbind(shape_edge, shape_label)
}
