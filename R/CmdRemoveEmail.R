#------------------------------------------------------------------------------#
#                            CmdRemoveEmail                                    #
#------------------------------------------------------------------------------#
#' CmdRemoveEmail
#'
#' \code{CmdRemoveEmail} Command for the RemoveEmail class.
#'
#' Class that encapsulates the command to execute an object of the RemoveEmail
#' class
#'
#' @usage CmdRemoveEmail$new()
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
CmdRemoveEmail <- R6::R6Class(
  classname = "CmdRemoveEmail",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdRemoveEmail"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveEmail$new(x)$execute()
      return(x)
    }
  )
)