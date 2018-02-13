#------------------------------------------------------------------------------#
#                                Strip Text                                    #
#------------------------------------------------------------------------------#
#' StripText
#'
#' \code{StripText}  Removes Unwanted Text
#'
#' A wrapper for \code{\link[textclean]{strip}} which strips text of unwanted characters.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage StripText$new(x, keepChars = NULL, removeDigits = TRUE, removeApostrophe = TRUE, lowerCase = TRUE)$execute()
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
#' @examples
#'
#' @return \code{StripText} Returns a vector with unwanted characters removed.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
StripText <- R6::R6Class(
  classname = "StripText",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Text0,

  private = list(

    ..keepChars = character(),
    ..removeDigits = logical(),
    ..removeApostrophe = logical(),
    ..lowerCase = logical(),

    processText = function(content) {
      content <- textclean::strip(x = content,
                                          char.keep = private$..keepChars,
                                          digit.remove = private$..removeDigits,
                                          apostrophe.remove = private$..removeApostrophe,
                                          lower.case = private$..lowerCase)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, keepChars = NULL, removeDigits = TRUE,
                          removeApostrophe = TRUE, lowerCase = TRUE) {
      private$..className <- "StripText"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "StripText"
      private$..x <- x
      private$..keepChars <- keepChars
      private$..removeDigits <- removeDigits
      private$..removeApostrophe <- removeApostrophe
      private$..lowerCase = lowerCase
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
