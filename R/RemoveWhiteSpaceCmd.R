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
#' @template textStudioParams
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
RemoveWhiteSpaceCmd <- R6::R6Class(
  classname = "RemoveWhiteSpaceCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

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
