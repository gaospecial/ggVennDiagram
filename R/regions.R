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

get_subset_items <- function(venn, specific = TRUE){
  n = length(venn@sets)
  c = combinations(n)
  fun = ifelse(specific, "discern_overlap", "overlap")
  lapply(c, fun, venn = venn)
}

get_subset_names <- function(venn, sep = "/"){
  n = length(venn@sets)
  set_name = venn@names
  c = combinations(n)
  sapply(c, function(i) paste0(set_name[i], collapse = sep))
}

get_subset_ids <- function(venn, sep = "/"){
  n = length(venn@sets)
  c = combinations(n)
  sapply(c, function(i) paste0(i, collapse = sep))
}


