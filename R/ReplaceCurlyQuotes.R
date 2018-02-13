#------------------------------------------------------------------------------#
#                           Replace Curly Quotes                               #
#------------------------------------------------------------------------------#
#' ReplaceCurlyQuotes
#'
#' \code{ReplaceCurlyQuotes}  Replaces curly single and double quotes.
#'
#' A wrapper for \code{\link[textclean]{replace_non_ascii}}
#' Replaces curly single and double quotes.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceCurlyQuotes$new(x, removeNonCoverted)$execute()
#'
#' @template textCleanParams
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceCurlyQuotes} Returns a vector with curly quotes replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceCurlyQuotes <- R6::R6Class(
  classname = "ReplaceCurlyQuotes",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Text0,

  private = list(

    processText = function(content) {
      Encoding(content) <- "latin1"
      content <- textclean::replace_curly_quote(x = content)
      return(content)
    }
  ),

  public = list(
    initialize = function(x) {
      private$..className <- "ReplaceCurlyQuotes"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceCurlyQuotes"
      private$..x <- x
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
