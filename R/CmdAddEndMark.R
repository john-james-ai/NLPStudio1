#------------------------------------------------------------------------------#
#                            CmdAddEndMark                                     #
#------------------------------------------------------------------------------#
#' CmdAddEndMark
#'
#' \code{CmdAddEndMark} Command for the AddEndMark class.
#'
#' Class that encapsulates the command to execute an object of the AddEndMark
#' class
#'
#' @usage CmdAddEndMark$new(replacement = "|", endmarks = c("?", ".", "!"))
#'
#' @template textCleanParams
#' @param replacement Symbol added for missing endmarks
#' @param endmarks List of endmark symbols to detect
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
CmdAddEndMark <- R6::R6Class(
  classname = "CmdAddEndMark",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,
  
  private = list(
    ..endmarks = character()
  ),

  public = list(
    initialize = function(replacement = "|", endmarks = c("?", ".", "!"), ...) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdAddEndMark"
      private$..replacement <- replacement
      private$..endmarks <- endmarks
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- AddEndMark$new(x, private$..replacement, private$..endmarks)$execute()
      return(x)
    }
  )
)
