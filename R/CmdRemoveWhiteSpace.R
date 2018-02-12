#------------------------------------------------------------------------------#
#                            CmdRemoveWhiteSpace                               #
#------------------------------------------------------------------------------#
#' CmdRemoveWhiteSpace
#'
#' \code{CmdRemoveWhiteSpace} Command for the RemoveWhiteSpace class.
#'
#' Class that encapsulates the command to execute an object of the RemoveWhiteSpace
#' class
#'
#' @usage CmdRemoveWhiteSpace$new()
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
CmdRemoveWhiteSpace <- R6::R6Class(
  classname = "CmdRemoveWhiteSpace",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdRemoveWhiteSpace"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveWhiteSpace$new(x)$execute()
      return(x)
    }
  )
)
