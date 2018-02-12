#------------------------------------------------------------------------------#
#                            CmdRemoveSymbols                                  #
#------------------------------------------------------------------------------#
#' CmdRemoveSymbols
#'
#' \code{CmdRemoveSymbols} Command for the RemoveSymbols class.
#'
#' Class that encapsulates the command to execute an object of the RemoveSymbols
#' class
#'
#' @usage CmdRemoveSymbols$new()
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
CmdRemoveSymbols <- R6::R6Class(
  classname = "CmdRemoveSymbols",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdRemoveSymbols"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveSymbols$new(x)$execute()
      return(x)
    }
  )
)
