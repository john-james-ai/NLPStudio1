#------------------------------------------------------------------------------#
#                         Replace Word Elongation                              #
#------------------------------------------------------------------------------#
#' ReplaceWordElongation
#'
#' \code{ReplaceWordElongation}  Replace Word Elongations.
#'
#' A wrapper for \code{\link[textclean]{replace_word_elongation}} In informal writing
#' people may use a form of text embellishment to emphasize or alter word meanings
#' called elongation (a.k.a. "word lengthening"). For example, the use of "Whyyyyy" conveys
#' frustration. Other times the usage may be to be more sexy (e.g., "Heyyyy there"). Other times it
#' may be used for emphasis (e.g., "This is so gooood"). This function uses an augmented form of
#' Armstrong & Fogarty’s (2007) algorithm. The algorithm first attempts to replace the elongation
#' with known semantic replacements (optional; default is FALSE). After this the algorithm locates all
#' places were the same letter (case insensitive) appears 3 times consecutively. These elements are then
#' further processed. The matches are replaces via fgsub by first taking the elongation to it’s canonical
#' form (drop all > 1 consecutive letters to a single letter) and then replacing with the most common
#' word used in 2008 in Google’s ngram data set that takes the canonical form. If the canonical form
#' is not found in the Google data set then the canonical form is used as the replacement.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceWordElongation$new(x, impartMeaning = FALSE)$execute()
#'
#' @template textCleanParams
#' @param impartMeaning logical. If TRUE, known elongation semantics are used as replacements
#' (see textclean:::meaning_elongations for known elongation semantics and replacements).
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceWordElongation} Returns a vector with word elongations replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceWordElongation <- R6::R6Class(
  classname = "ReplaceWordElongation",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Text0,

  private = list(

    ..impartMeaning = logical(),

    processText = function(content) {
      content <- textclean::replace_word_elongation(x = content,
                                           impart.meaning = private$..impartMeaning)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, impartMeaning = FALSE) {
      private$..className <- "ReplaceWordElongation"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceWordElongation"
      private$..x <- x
      private$..impartMeaning <- impartMeaning
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
