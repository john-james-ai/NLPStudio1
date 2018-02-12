#------------------------------------------------------------------------------#
#                               CmdLowerCase                                   #
#------------------------------------------------------------------------------#
#' CmdLowerCase
#'
#' \code{CmdLowerCase} Command for the LowerCase class.
#'
#' Class that encapsulates the command to execute an object of the LowerCase
#' class
#'
#' @usage CmdLowerCase$new(keepChars = NULL, removeDigits = TRUE, lowerCase = TRUE)
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
CmdLowerCase <- R6::R6Class(
  classname = "CmdLowerCase",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  public = list(
    initialize = function(keepChars = NULL, removeDigits = TRUE,
                          removeApostrophe = TRUE, lowerCase = TRUE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdLowerCase"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- LowerCase$new(x)$execute()
      return(x)
    }
  )
)
