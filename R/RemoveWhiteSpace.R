#------------------------------------------------------------------------------#
#                            Remove Extra White Space                          #
#------------------------------------------------------------------------------#
#' RemoveWhiteSpace
#'
#' \code{RemoveWhiteSpace} Removes extra white space from text.
#'
#' @usage RemoveWhiteSpace$new(x)$execute()
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
RemoveWhiteSpace <- R6::R6Class(
  classname = "RemoveWhiteSpace",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  public = list(
    initialize = function(x) {
      private$..className <- "RemoveWhiteSpace"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveWhiteSpace"
      private$..x <- x
      private$..regex <- "\\s+"
      private$..replacement <- " "
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
