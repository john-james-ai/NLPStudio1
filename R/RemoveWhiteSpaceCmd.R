#------------------------------------------------------------------------------#
#                            RemoveWhiteSpaceCmd                               #
#------------------------------------------------------------------------------#
#' RemoveWhiteSpaceCmd
#'
#' \code{RemoveWhiteSpaceCmd} Command for the RemoveWhiteSpace class.
#'
#' Class that encapsulates the command to execute an object of the RemoveWhiteSpace
#' class
#'
#' @usage RemoveWhiteSpaceCmd$new()
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
RemoveWhiteSpaceCmd <- R6::R6Class(
  classname = "RemoveWhiteSpaceCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCmd0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "RemoveWhiteSpaceCmd"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveWhiteSpace$new(x)$execute()
      return(x)
    }
  )
)
