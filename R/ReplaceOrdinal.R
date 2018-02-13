#------------------------------------------------------------------------------#
#                             Replace Ordinal                                  #
#------------------------------------------------------------------------------#
#' ReplaceOrdinal
#'
#' \code{ReplaceOrdinal}  Replace Mixed Ordinal Numbers With Text Representation
#'
#' A wrapper for \code{\link[textclean]{replace_ordinal}} Replaces mixed text/numeric
#' represented ordinal numbers with words (e.g., "1st" becomes "first").
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceOrdinal$new(x)$execute()
#' @usage ReplaceOrdinal$new(x, joinOrdinal = TRUE, remove = FALSE)$execute()
#'
#' @template textCleanParams
#' @param joinOrdinal Logical. If FALSE the elements of larger numbers are separated with spaces. If TRUE the elements will be joined without spaces.
#' @param remove Logical. If TRUE numbers are removed from the text.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceOrdinal} Returns a vector with ordinals replaced or removed.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceOrdinal <- R6::R6Class(
  classname = "ReplaceOrdinal",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Text0,

  private = list(

    ..joinOrdinal = logical(),
    ..remove = logical(),

    processText = function(content) {
      content <- textclean::replace_number(x = content,
                                            num.paste = private$..joinOrdinal,
                                            remove = private$..remove)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, joinOrdinal = FALSE, remove = FALSE) {
      private$..className <- "ReplaceOrdinal"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceOrdinal"
      private$..x <- x
      private$..joinOrdinal <- joinOrdinal
      private$..remove <- remove
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
