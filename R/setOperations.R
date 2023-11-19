# Note: These codes were transferred from RVenn 1.1.0.
# The author is Turgut Yigit Akyol.


# Method for intersection =======================


#' Intersection of many sets.
#'
#' \code{overlap} returns the same elements of the sets in a \code{Venn} object.
#'
#' @param venn (Required) A \code{Venn} object.
#' @param slice (Optional) The names or the indices of sets of interest. Default
#'   is "all", meaning the intersection will be calculated for all the sets.
#' @return A vector showing the intersection of the sets.
#' @examples
#' venn = Venn(list(letters[1:10], letters[3:12], letters[6:15]))
#' overlap(venn)
#' overlap(venn, slice = c(1, 2))
#' @name overlap
#' @author tyakyol@gmail.com
setGeneric("overlap", function(venn, slice = "all") {
  standardGeneric("overlap")
}
)

#' @export
#' @rdname overlap
setMethod("overlap", c(venn = "Venn", slice = "ANY"),
          function(venn, slice = "all") {

            if (slice[1] != "all") {
              venn2 = venn@sets[slice]
              inter = purrr::reduce(venn2, function(x, y) intersect(x, y))
            } else {
              inter = purrr::reduce(venn@sets, function(x, y) intersect(x, y))
            }

            inter
          }
)


# Method for union ==============================

#' Union of many sets.
#'
#' \code{unite} returns the union of the sets in a \code{Venn} object.
#'
#' @param venn (Required) A \code{Venn} object.
#' @param slice (Optional) The names or the indices of sets of interest. Default
#'   is "all", meaning the union will be calculated for all the sets.
#' @return A vector showing the union of the sets.
#' @examples
#' venn = Venn(list(letters[1:10], letters[3:12], letters[6:15]))
#' unite(venn)
#' unite(venn, slice = c(1, 2))
#' @name unite
#' @author tyakyol@gmail.com
setGeneric("unite", function(venn, slice = "all") {
  standardGeneric("unite")
}
)

#' @export
#' @rdname unite
setMethod("unite", c(venn = "Venn", slice = "ANY"),
          function(venn, slice = "all") {

            if (slice[1] != "all") {
              venn2 = venn@sets[slice]
              uni = purrr::reduce(venn2, function(x, y) union(x, y))
            } else {
              uni = purrr::reduce(venn@sets, function(x, y) union(x, y))
            }

            uni
          }
)


# Method for difference =========================


#' Set difference.
#'
#' \code{discern} returns the difference between two group of sets selected from
#'   a \code{Venn} object. If multiple sets are chosen for the slices, union of
#'   those sets will be used.
#'
#' @param venn (Required) A \code{Venn} object.
#' @param slice1 (Required) The name or the index of the set of interest.
#' Multiple sets can be selected.
#' @param slice2 (Optional) The name or the index of the set of interest.
#'   Multiple sets can be selected. Default is all the sets except the sets of
#'   slice1.
#' @return A vector showing the difference between slice1 and slice2.
#' @examples
#' venn = Venn(list(letters[1:10], letters[3:12], letters[6:15]))
#' discern(venn, slice1 = 1)
#' discern(venn, slice1 = c(1, 2), slice2 = 3)
#' @name discern
#' @author tyakyol@gmail.com
setGeneric("discern", function(venn,
                               slice1,
                               slice2 = "all") {
  standardGeneric("discern")
}
)

#' @export
#' @rdname discern
setMethod("discern", c(venn = "Venn", slice1 = "ANY", slice2 = "ANY"),
          function(venn,
                   slice1,
                   slice2 = "all") {

            if (is.numeric(slice1)) {
              slice1 = names(venn@sets)[slice1]
            }

            if (is.numeric(slice2)) {
              slice2 = names(venn@sets)[slice2]
            }

            if (slice2[1] == "all") {
              slice2 = setdiff(names(venn@sets), slice1)
              set1 = venn@sets[slice1] |> purrr::reduce(function(x, y) union(x, y))
              set2 = venn@sets[slice2] |> purrr::reduce(function(x, y) union(x, y))
              differ = setdiff(set1, set2)
            } else {
              set1 = venn@sets[slice1] |> purrr::reduce(function(x, y) union(x, y))
              set2 = venn@sets[slice2] |> purrr::reduce(function(x, y) union(x, y))
              differ = setdiff(set1, set2)
            }

            differ
          }
)
