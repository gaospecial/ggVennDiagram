################## method for polygon intersection  ############

#' calculate the overlap region of `Polygon` object
#'
#' @inheritParams discern_overlap
#'
#' @export
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


################ Method for polygon difference ############

#' calculate the difference region of `Polygon` object
#'
#' @param venn Venn object
#' @param slice1 first slice of Venn object
#' @param slice2 second slice of Venn object, default is all except the first slice
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom sf st_difference st_union
setMethod("discern", c(venn = "Polygon", slice1 = "ANY", slice2 = "ANY"),
          function(venn,
                   slice1,
                   slice2 = "all") {
            polygon = venn
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


#' calculate region of Venn
#'
#' @param venn a Venn object
#' @param slice index of Venn members, default is "all"
#'
#' @return region items
#' @export
#' @name discern_overlap
#'
#' @examples
#' library(ggVennDiagram)
#' venn <- Venn(list(A=1:3,B=2:5,C=c(1L,3L,5L)))
#'
#' discern_overlap(venn, slice = "all")
#' # is equal to
#' overlap(venn, slice = "all")
#'
#' # however, `discern_overlap()` only contains specific region
#' discern_overlap(venn, slice = 1:2)
#'
setGeneric("discern_overlap", function(venn, slice = "all") standardGeneric("discern_overlap"))

#' calculate the unique region defined by `Venn` object and the parameter `slice`
#'
#' @param venn Venn object
#' @param slice a numeric vector indicating the index of slice, default is "all"
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
              return(sf::st_intersection(overlap, discern))
            }
          })


