#------------------------------------------------------------------------------#
#                              Remove Hyphens                                  #
#------------------------------------------------------------------------------#
#' RemoveHyphens
#'
#' \code{RemoveHyphens} Removes hyphens from text.
#'
#' @usage RemoveHyphens$new(x)$execute()
#'
#' @template textStudioParams
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
RemoveHyphens <- R6::R6Class(
  classname = "RemoveHyphens",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Text0,

  public = list(
    initialize = function(x) {
      private$..className <- "RemoveHyphens"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveHyphens"
      private$..x <- x
      private$..regex <- '[-]'
      private$..replacement <- " "
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
