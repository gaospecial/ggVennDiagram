############ Class Venn #####################
#
# The Venn class is defined originally in RVenn package by *Turgut Yigit Akyol*.
# It set a good model in calculating the subsets between different sets.
# However, RVenn has not been updated since 2019. I communicate with
# its author and he agreed to transfer some of the codes from RVenn to
# ggVennDiagram.

#' *`Venn`* is a S4 class to represent multiple sets.
#' @md
#' @slot sets A \code{list} object containing vectors in the same type.
#' @slot names The names of the \code{sets} if it has names. If the \code{list}
#'   doesn't have names, the sets will be named as "Set_1", "Set_2", "Set_3" and
#'   so on.
#' @name Venn-class
setClass("Venn",
         slots = list(sets = "ANY", names = "ANY")
)


######### Venn Constructor #####

#' @title `Venn()` is a Venn class object constructor.
#'
#' @param sets (Required) A list containing vectors in the same class. If a
#'   vector contains duplicates they will be discarded. If the list doesn't have
#'   names the sets will be named as "Set_1", "Set_2", "Set_3" and so on.
#' @param names names of sets
#'
#' @return A `Venn` object.
#' @rdname Venn-class
#' @docType methods
#' @export
#' @md
#' @examples
#'  venn = Venn(list(letters[1:10], letters[3:12], letters[6:15]))
#'  print(venn)
setGeneric("Venn", function(sets, names = NULL){
  standardGeneric("Venn")
})


#' @export
#' @rdname Venn-class
#' @importFrom methods new
setMethod("Venn", c(sets = "ANY", names = "ANY"),
          function(sets, names = NULL){
            # validate parameters
            if (!is.list(sets)) {
              stop("Data should be given in a list.")
            }

            if (length(sets) <= 1) {
              stop("The list should contain at least 2 vectors.")
            }

            if (!all_identical(lapply(sets, class))){
              stop("All sets should have same classes.")
            }

            # check and assign valid set names
            if (!is.null(names)){
              if (length(sets) != length(names)) stop("Lengths of sets and names are not equal.")
            } else if (is.null(names(sets))){
              names = paste("Set", seq_len(length(sets)), sep = "_")
            } else {
              names = names(sets)
            }

            # remove duplicates
            sets = lapply(sets, unique)

            # constructor
            data = new(Class = "Venn",
                       sets = sets,
                       names = names)

            return(data)
          })

######## Helper functions ########

#' All members of a list have the same elements
#'
#' @param list a list
#' @return TRUE or FALSE
all_identical = function(list){
  if (!is.list(list)) stop("Input should be a list.")
  n = length(list)
  if (n <= 1){
    warning("list has less than 2 items.")
    invisible()
  } else if (n <= 2){
    identical(list[[1]], list[[2]])
  } else {
    all(sapply(list[-1], identical, x = list[[1]]))
  }
}

#' method for S4 Venn object
#'
#' Print user-friendly information of a Venn object
#'
#' @param object a Venn class object
#' @export
#' @method show Venn
#' @importFrom methods show slotNames slot
#' @rdname Venn-class
setMethod("show", c(object = "Venn"),
          function(object){
            cat("An object of class 'Venn':\n")
            cat("   Slots: ", paste0(slotNames(object), collapse = ", "), ";\n", sep = "")
            cat("   No. Sets:", length(object@sets))
            cat("   SetNames: ", paste0(slot(object, "names"), collapse = ", "), ".\n", sep = "")
})

