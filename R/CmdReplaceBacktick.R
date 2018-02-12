#------------------------------------------------------------------------------#
#                           CmdReplaceBacktick                                 #
#------------------------------------------------------------------------------#
#' CmdReplaceBacktick
#'
#' \code{CmdReplaceBacktick} Command for the ReplaceBacktick class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceBacktick
#' class
#'
#' @usage CmdReplaceBacktick$new()
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
CmdReplaceBacktick <- R6::R6Class(
  classname = "CmdReplaceBacktick",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,


  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdReplaceBacktick"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceBacktick$new(x)$execute()
      return(x)
    }
  )
)
