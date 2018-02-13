#------------------------------------------------------------------------------#
#                              Add Comma Space                                 #
#------------------------------------------------------------------------------#
#' AddCommaSpace
#'
#' \code{AddCommaSpace} Adds space after comma.
#'
#' Class adds space after comma when commas are followed by non space characters.
#'
#' @usage AddCommaSpace$new(x)$execute()
#'
#' @template textCleanParams
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
AddCommaSpace <- R6::R6Class(
  classname = "AddCommaSpace",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  public = list(
    initialize = function(x) {
      private$..className <- "AddCommaSpace"
      private$..methodName <- "initialize"
      private$..x <- x
      private$..meta[["name"]] <-  "AddCommmaSpace"
      private$..regex <- "(,)([^ ])"
      private$..replacement <- "\\1 \\2"
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
