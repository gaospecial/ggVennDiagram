# region items and polygons

#' all possible combinations of n sets
#'
#' @param n dim
#'
#' @importFrom utils combn
combinations <- function(n){
  l <- lapply(seq_len(n), function(x){
    m <- combn(n,x)
    matrix2list(m)
  })
  unlist(l, recursive = F)
}

matrix2list <- function(matrix){
  lapply(seq_len(ncol(matrix)), function(i) matrix[,i])
}

get_region_items <- function(venn){
  n = length(venn@sets)
  c = combinations(n)
  lapply(c, function(i) discern_overlap(venn,i))
}

get_region_names <- function(venn){
  n = length(venn@sets)
  set_name = venn@names
  c = combinations(n)
  sapply(c, function(i) paste0(set_name[i], collapse = ".."))
}

get_region_ids <- function(venn){
  n = length(venn@sets)
  c = combinations(n)
  sapply(c, function(i) paste0(i, collapse = ""))
}


