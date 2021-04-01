# method for polygon intersection


#' @export
#' @rdname overlap
setMethod("overlap", c(venn = "Polygon", slice = "ANY"),
          function(venn, slice = "all"){
            if (slice[1] != "all"){
              polygon2 = venn@sets[slice]
              inter = purrr::reduce(polygon2, function(x,y) sf::st_intersection(x,y))
            } else {
              inter = purrr::reduce(venn@sets, function(x,y) sf::st_intersection(x,y))
            }
            return(inter)
          })


# Method for polygon difference =========================
#' @export
#' @importFrom magrittr %>%
#' @importFrom sf st_difference st_union
#' @rdname discern
setMethod("discern", c(venn = "Polygon", slice1 = "ANY", slice2 = "ANY"),
          function(venn,
                   slice1,
                   slice2 = "all") {

            if (is.numeric(slice1)) {
              slice1 = names(polygon@sets)[slice1]
            }

            if (is.numeric(slice2)) {
              slice2 = names(polygon@sets)[slice2]
            }

            if (slice2[1] == "all") {
              slice2 = setdiff(names(polygon@sets), slice1)
              set1 = polygon@sets[slice1] %>% purrr::reduce(function(x, y) st_union(x, y))
              set2 = polygon@sets[slice2] %>% purrr::reduce(function(x, y) st_union(x, y))
              differ = st_difference(set1, set2)
            } else {
              set1 = polygon@sets[slice1] %>% purrr::reduce(function(x, y) st_union(x, y))
              set2 = polygon@sets[slice2] %>% purrr::reduce(function(x, y) st_union(x, y))
              differ = st_difference(set1, set2)
            }

            differ
          }
)


setGeneric("discern_overlap", function(venn, slice = "all") standardGeneric("discern_overlap"))

#' @export
#' @rdname discern_overlap
setMethod("discern_overlap", c(venn="Venn", slice="ANY"),
          function(venn, slice = "all"){
            overlap = RVenn::overlap(venn, slice = slice)
            if (slice[1] == "all" | identical(venn@sets[slice], venn@sets)){
              discern = NULL
              return(overlap)
            } else {
              discern = RVenn::discern(venn, slice1 = slice)
              return(intersect(overlap, discern))
            }
          })

#' @export
#' @rdname discern_overlap
setMethod("discern_overlap", c(venn="Polygon", slice="ANY"),
          function(venn, slice = "all"){
            overlap = overlap(venn, slice = slice)
            if (slice[1] == "all" | identical(venn@sets[slice], venn@sets)){
              discern = NULL
              return(overlap)
            } else {
              discern = discern(venn, slice1 = slice)
              return(st_intersection(overlap, discern))
            }
          })


