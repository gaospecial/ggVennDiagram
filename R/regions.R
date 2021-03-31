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

setGeneric("discern_overlap", function(venn, slice = "all"){
  standardGeneric("discern_overlap")
})

#' @export
#' @rdname region_item
setMethod("discern_overlap", c(venn="Venn", slice="ANY"),
          function(venn, slice = "all"){
            overlap = RVenn::overlap(venn, slice = slice)
            if (identical(venn@sets[slice], venn@sets)){
              discern = NULL
              return(overlap)
            } else {
              discern = RVenn::discern(venn, slice1 = slice)
              return(intersect(overlap, discern))
            }
          })
