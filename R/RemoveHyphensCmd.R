#------------------------------------------------------------------------------#
#                            RemoveHyphensCmd                                  #
#------------------------------------------------------------------------------#
#' RemoveHyphensCmd
#'
#' \code{RemoveHyphensCmd} Command for the RemoveHyphens class.
#'
#' Class that encapsulates the command to execute an object of the RemoveHyphens
#' class
#'
#' @usage RemoveHyphensCmd$new()
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
RemoveHyphensCmd <- R6::R6Class(
  classname = "RemoveHyphensCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCmd0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "RemoveHyphensCmd"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveHyphens$new(x)$execute()
      return(x)
    }
  )
)
