#------------------------------------------------------------------------------#
#                               CmdReplaceKern                                 #
#------------------------------------------------------------------------------#
#' CmdReplaceKern
#'
#' \code{CmdReplaceKern} Command for the ReplaceKern class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceKern
#' class
#'
#' @usage CmdReplaceKern$new()
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
CmdReplaceKern <- R6::R6Class(
  classname = "CmdReplaceKern",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdReplaceKern"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceKern$new(x)$execute()
      return(x)
    }
  )
)
