#------------------------------------------------------------------------------#
#                               CmdStripText                                   #
#------------------------------------------------------------------------------#
#' CmdStripText
#'
#' \code{CmdStripText} Command for the StripText class.
#'
#' Class that encapsulates the command to execute an object of the StripText
#' class
#'
#' @usage CmdStripText$new(keepChars = NULL, removeDigits = TRUE, lowerCase = TRUE)
#'
#' @template textCleanParams
#' @param keepChars A character vector of symbols (i.e., punctuation) that
#' strip should keep. The default is to strip every symbol except apostrophes
#' and a double tilde "~~". The double tilde "~~" is included for a
#' convenient means of keeping word groups together in functions that
#' split text apart based on space To remove double tildes "~~" set
#' char.keep to NULL.
#' @param removeDigits Logical. If TRUE, digits are removed from the text.
#' @param removeApostrophe Logical. If TRUE, digits are removed from the text.
#' @param lowerCase Logical.  If TRUE, forces all alphabetic characters to lower case.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
CmdStripText <- R6::R6Class(
  classname = "CmdStripText",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  private = list(
    ..keepChars = character(),
    ..removeDigits = logical(),
    ..removeApostrophe = logical(),
    ..lowerCase = logical()
  ),

  public = list(
    initialize = function(keepChars = NULL, removeDigits = TRUE,
                          removeApostrophe = TRUE, lowerCase = TRUE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdStripText"
      private$..keepChars <- keepChars
      private$..removeDigits <- removeDigits
      private$..removeApostrophe <- removeApostrophe
      private$..lowerCase = lowerCase
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- StripText$new(x, keepChars = private$..keepChars,
                         removeDigits = private$..removeDigits,
                         removeApostrophe = private$..removeApostrophe,
                         lowerCase = private$..lowerCase)$execute()
      return(x)
    }
  )
)
