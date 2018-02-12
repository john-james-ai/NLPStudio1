#------------------------------------------------------------------------------#
#                               CmdReplaceNonAscii                             #
#------------------------------------------------------------------------------#
#' CmdReplaceNonAscii
#'
#' \code{CmdReplaceNonAscii} Command for the ReplaceNonAscii class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceNonAscii
#' class
#'
#' @usage CmdReplaceNonAscii$new(removeNonConverted = FALSE)
#'
#' @template textCleanParams
#' @param removeNonConverted Logical. If TRUE unmapped encodings are deleted from the string.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
CmdReplaceNonAscii <- R6::R6Class(
  classname = "CmdReplaceNonAscii",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  private = list(
    ..removeNonConverted = logical()
  ),

  public = list(
    initialize = function(removeNonConverted = TRUE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdReplaceNonAscii"
      private$..removeNonConverted <- removeNonConverted
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceNonAscii$new(x, removeNonConverted = private$..removeNonConverted)$execute()
      return(x)
    }
  )
)
