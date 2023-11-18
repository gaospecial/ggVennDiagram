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

#' @rdname Venn-method
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
