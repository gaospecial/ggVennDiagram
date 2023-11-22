############ Class Venn #####################
#
# The Venn class is defined originally in RVenn package by **.
# It set a good model in caculating the subsets between different sets.
# However, RVenn has not been updated since 2019. I communicate with
# its author and agreed to transfer some of the codes from RVenn to
# ggVennDiagram.

#' An S4 class to represent multiple sets.
#'
#' This class is adopted from `RVenn`. Since `RVenn` doesn't export this class,
#' I have to copy its codes hereafter to use it.
#'
#' @slot sets A \code{list} object containing vectors in the same type.
#' @slot names The names of the \code{sets} if it has names. If the \code{list}
#'   doesn't have names, the sets will be named as "Set_1", "Set_2", "Set_3" and
#'   so on.
#' @name Venn-class
setClass("Venn",
         slots = list(sets = "ANY", names = "ANY")
)



#' Venn class object constructor
#'
#' @param sets a list of sets
#' @param names names of sets
#' @name Venn-method
#' @export
setGeneric("Venn", function(sets, names){
  standardGeneric("Venn")
})

#' Build a \code{Venn} object.
#'
#' \code{Venn} builds a \code{Venn} object from a list.
#'
#' @param sets (Required) A list containing vectors in the same class. If a
#'   vector contains duplicates they will be discarded. If the list doesn't have
#'   names the sets will be named as "Set_1", "Set_2", "Set_3" and so on.
#' @return A \code{Venn} object.
#' @examples
#' venn = Venn(list(letters[1:10], letters[3:12], letters[6:15]))
#' print(venn)
#' @name Venn-method
#' @importFrom methods new
setMethod("Venn", c(sets = "ANY", names = "ANY"),
          function(sets, names){
            # validate parameters


            # constructor
            data = new(Class = "Venn",
                       sets = sets,
                       names = names)
            data
          })


#' @export
#' @importFrom methods new
#' @rdname Venn-method
setMethod("Venn", c(sets = "ANY"),
          function(sets) {

            if (!is.list(sets)) {
              stop("Data should be given in a list.")
            }

            if (sum(sapply(sets, is.null) == TRUE) >= 1) {
              sets = sets[!(sapply(sets, is.null))]
            }

            if (length(sets) <= 1) {
              stop("The list should contain at least 2 vectors.")
            }

            if (length(unique(sapply(sets, class))) != 1) {
              stop("Vectors should be in the same class.")
            }

            if (!(sapply(sets, class)[1] %in% c("integer", "numeric", "character"))) {
              stop("The list must contain only integers, numerics or characters.")
            }

            venn = new(Class = "Venn", sets = sets)

            if (is.null(names(venn@sets))) {
              names(venn@sets) = paste("Set", seq_len(length(venn@sets)), sep = "_")
            }

            venn@names = names(venn@sets)

            venn@sets = lapply(venn@sets, unique)  # Sets shouldn't include duplicates.

            venn
          }
)
