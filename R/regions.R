# region items and polygons

combinations <- function(n){
  l <- lapply(seq_len(n), function(x){
    m <- combn(n,x)
    matrix2list(m)
  })
  unlist(l, recursive = F)
}

matrix2list <- function(m){
  lapply(seq_len(ncol(m)), function(i) m[,i])
}

region_items <- function(venn){
  n = length(venn@sets)
  c = combinations(n)
  lapply(c, function(i) discern_overlap(venn,i))
}

region_names <- function(venn){
  n = length(venn@sets)
  set_name = venn@names
  c = combinations(n)
  lapply(c, function(i) paste0(set_name[i], collapse = ".."))
}

region_id <- function(venn){
  n = length(venn@sets)
  c = combinations(n)
  sapply(c, function(i) paste0(i, collapse = ""))
}


