#------------------------------------------------------------------------------#
#                             CmdRemoveURL                                     #
#------------------------------------------------------------------------------#
#' CmdRemoveURL
#'
#' \code{CmdRemoveURL} Command for the RemoveURL class.
#'
#' Class that encapsulates the command to execute an object of the RemoveURL
#' class
#'
#' @usage CmdRemoveURL$new()
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
CmdRemoveURL <- R6::R6Class(
  classname = "CmdRemoveURL",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdRemoveURL"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveURL$new(x)$execute()
      return(x)
    }
  )
)
