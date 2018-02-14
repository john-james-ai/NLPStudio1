#------------------------------------------------------------------------------#
#                               LowerCaseCmd                                   #
#------------------------------------------------------------------------------#
#' LowerCaseCmd
#'
#' \code{LowerCaseCmd} Command for the LowerCase class.
#'
#' Class that encapsulates the command to execute an object of the LowerCase
#' class
#'
#' @usage LowerCaseCmd$new(keepChars = NULL, removeDigits = TRUE, lowerCase = TRUE)
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
LowerCaseCmd <- R6::R6Class(
  classname = "LowerCaseCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCmd0,

  public = list(
    initialize = function(keepChars = NULL, removeDigits = TRUE,
                          removeApostrophe = TRUE, lowerCase = TRUE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "LowerCaseCmd"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- LowerCase$new(x)$execute()
      return(x)
    }
  )
)
