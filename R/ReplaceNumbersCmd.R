#------------------------------------------------------------------------------#
#                               ReplaceNumbersCmd                              #
#------------------------------------------------------------------------------#
#' ReplaceNumbersCmd
#'
#' \code{ReplaceNumbersCmd} Command for the ReplaceNumbers class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceNumbers
#' class
#'
#' @usage ReplaceNumbersCmd$new(joinNumbers = FALSE, remove = FALSE)
#'
#' @template textStudioParams
#' @param joinNumbers Logical. If FALSE the elements of larger numbers are separated with spaces. If TRUE the elements will be joined without spaces.
#' @param remove Logical. If TRUE numbers are removed from the text.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceNumbersCmd <- R6::R6Class(
  classname = "ReplaceNumbersCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..joinNumbers = logical(),
    ..remove = logical()
  ),

  public = list(
    initialize = function(joinNumbers = FALSE, remove = FALSE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "ReplaceNumbersCmd"
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
