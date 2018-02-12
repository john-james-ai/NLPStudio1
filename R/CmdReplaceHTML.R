#------------------------------------------------------------------------------#
#                             CmdReplaceHTML                                   #
#------------------------------------------------------------------------------#
#' CmdReplaceHTML
#'
#' \code{CmdReplaceHTML} Command for the ReplaceHTML class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceHTML
#' class
#'
#' @usage CmdReplaceHTML$new(symbol = FALSE)
#'
#' @template textCleanParams
#' @param symbol Logical. If codeTRUE the symbols are retained with appropriate replacements.
#' If FALSE they are removed.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
CmdReplaceHTML <- R6::R6Class(
  classname = "CmdReplaceHTML",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  private = list(
    ..symbol = logical()
  ),

  public = list(
    initialize = function(symbol = TRUE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdReplaceHTML"
      private$..symbol <- symbol
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceHTML$new(x, private$..symbol)$execute()
      return(x)
    }
  )
)
