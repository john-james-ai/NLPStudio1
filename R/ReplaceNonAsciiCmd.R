#------------------------------------------------------------------------------#
#                               ReplaceNonAsciiCmd                             #
#------------------------------------------------------------------------------#
#' ReplaceNonAsciiCmd
#'
#' \code{ReplaceNonAsciiCmd} Command for the ReplaceNonAscii class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceNonAscii
#' class
#'
#' @usage ReplaceNonAsciiCmd$new(removeNonConverted = FALSE)
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
ReplaceNonAsciiCmd <- R6::R6Class(
  classname = "ReplaceNonAsciiCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCmd0,

  private = list(
    ..removeNonConverted = logical()
  ),

  public = list(
    initialize = function(removeNonConverted = TRUE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "ReplaceNonAsciiCmd"
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
