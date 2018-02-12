#------------------------------------------------------------------------------#
#                               CmdReplaceNumbers                              #
#------------------------------------------------------------------------------#
#' CmdReplaceNumbers
#'
#' \code{CmdReplaceNumbers} Command for the ReplaceNumbers class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceNumbers
#' class
#'
#' @usage CmdReplaceNumbers$new(joinNumbers = FALSE, remove = FALSE)
#'
#' @template textCleanParams
#' @param joinNumbers Logical. If FALSE the elements of larger numbers are separated with spaces. If TRUE the elements will be joined without spaces.
#' @param remove Logical. If TRUE numbers are removed from the text.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
CmdReplaceNumbers <- R6::R6Class(
  classname = "CmdReplaceNumbers",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  private = list(
    ..joinNumbers = logical(),
    ..remove = logical()
  ),

  public = list(
    initialize = function(joinNumbers = FALSE, remove = FALSE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdReplaceNumbers"
      private$..joinNumbers <- joinNumbers
      private$..remove <- remove
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceNumbers$new(x, joinNumbers = private$..joinNumbers,
                              remove = private$..remove)$execute()
      return(x)
    }
  )
)
