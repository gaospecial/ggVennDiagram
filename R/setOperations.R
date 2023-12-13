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
              inter = Reduce(intersect, venn2)
            } else {
              inter = Reduce(intersect, venn@sets)
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
#'   venn = Venn(list(letters[1:10], letters[3:12], letters[6:15]))
#'   unite(venn)
#'   unite(venn, slice = c(1, 2))
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
            slice = slice_idx(venn, slice)
            if (slice[1] != "all") {
              venn2 = venn@sets[slice]
              uni = Reduce(union, venn2)
            } else {
              uni = Reduce(union, venn@sets)
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
            slice1 = slice_idx(venn, slice1)
            slice2 = slice_idx(venn, slice2)

            if (slice2[1] == "all") {
              set1 = Reduce(union, venn@sets[slice1])
              set2 = Reduce(union, venn@sets[-slice1])
              differ = setdiff(set1, set2)
            } else {
              set1 = Reduce(union, venn@sets[slice1])
              set2 = Reduce(union, venn@sets[slice2])
              differ = setdiff(set1, set2)
            }

            differ
          }
)


# Method for specific region =============

#' Calculate region of sets
#'
#' calculate the unique region defined
#' by `Venn` object and the parameter `slice`.
#'
#' @param venn a Venn object
#' @param slice index of Venn members, default is "all"
#'
#' @return region items
#' @export
#' @name discern_overlap
#' @author gaospecial@gmail.com
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
#' # is different from
#' overlap(venn, slice = 1:2)
setGeneric("discern_overlap", function(venn, slice = "all") standardGeneric("discern_overlap"))

#' @export
#' @rdname discern_overlap
setMethod("discern_overlap", c(venn="Venn", slice="ANY"),
          function(venn, slice = "all"){
            slice = slice_idx(venn, slice)
            overlap = overlap(venn, slice = slice)
            if (slice[1] == "all" | identical(venn@sets[slice], venn@sets)){
              discern = NULL
              return(overlap)
            } else {
              discern = discern(venn, slice1 = slice, slice2 = "all")
              return(intersect(overlap, discern))
            }
          })


#' check and format slice name
#'
#' @param venn a Venn object
#' @param slice a numeric or character vector
#'
#' @return the index of Venn (numeric vector) or "all"
slice_idx = function(venn, slice){
  set_name = venn@names
  if (is.numeric(slice)){
    found = slice %in% seq_along(set_name)
    if (all(found)){
      return(slice)
    } else {
      stop(paste("slice is not valid:", slice[!found]))
    }
  }
  if (is.character(slice)){
    if (any(slice == "all")){
      return("all")
    } else {
      matches = match(slice, set_name)
      if (all(!is.na(matches))){
        slice = matches
        return(slice)
      } else {
        non_exist_item = slice[is.na(matches)]
        stop(paste(non_exist_item, "is not found in this object."))
      }
    }
  }
  stop("slice should either be a character or numeric vector.")
}
