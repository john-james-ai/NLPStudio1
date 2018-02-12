#------------------------------------------------------------------------------#
#                            CmdRemoveHyphens                                  #
#------------------------------------------------------------------------------#
#' CmdRemoveHyphens
#'
#' \code{CmdRemoveHyphens} Command for the RemoveHyphens class.
#'
#' Class that encapsulates the command to execute an object of the RemoveHyphens
#' class
#'
#' @usage CmdRemoveHyphens$new()
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
CmdRemoveHyphens <- R6::R6Class(
  classname = "CmdRemoveHyphens",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdRemoveHyphens"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveHyphens$new(x)$execute()
      return(x)
    }
  )
)
