#------------------------------------------------------------------------------#
#                         Replace Contractions                                 #
#------------------------------------------------------------------------------#
#' ReplaceContractions
#'
#' \code{ReplaceContractions}  Replace contractions.
#'
#' A wrapper for \code{\link[textclean]{replace_contraction}} that
#' replaces contractions with long form.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceContractions$new(x, contractions = lexicon::key_contractions, ignoreCase = TRUE)$execute()
#'
#' @template textCleanParams
#' @param contractions A two column hash of contractions (column 1) and expanded
#' form replacements (column 2).  Default is to use \code{\link[lexicon]{key_contractions}} data set.
#' @param ignoreCase logical.  Should case be ignored?
#' @param \dots ignored.
#'
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceContractions} Returns a vector with contractions replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceContractions <- R6::R6Class(
  classname = "ReplaceContractions",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(
    ..contractions = character(),
    ..ignoreCase = logical(),

    processText = function(content) {
      content <- textclean::replace_contraction(x = content,
                                                 contraction.key = private$..contractions,
                                                 ignore.case = private$..ignoreCase)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, contractions = NULL,
                          ignoreCase = TRUE) {
      private$..className <- "ReplaceContractions"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceContractions"
      private$..x <- x
      private$..contractions <- contractions
      private$..ignoreCase <- ignoreCase
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
