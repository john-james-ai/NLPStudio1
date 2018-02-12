#------------------------------------------------------------------------------#
#                            CmdRemoveNumbers                                  #
#------------------------------------------------------------------------------#
#' CmdRemoveNumbers
#'
#' \code{CmdRemoveNumbers} Command for the RemoveNumbers class.
#'
#' Class that encapsulates the command to execute an object of the RemoveNumbers
#' class
#'
#' @usage CmdRemoveNumbers$new()
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
CmdRemoveNumbers <- R6::R6Class(
  classname = "CmdRemoveNumbers",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdRemoveNumbers"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveNumbers$new(x)$execute()
      return(x)
    }
  )
)
