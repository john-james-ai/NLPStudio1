#------------------------------------------------------------------------------#
#                             Replace Numbers                                  #
#------------------------------------------------------------------------------#
#' ReplaceNumbers
#'
#' \code{ReplaceNumbers}  Replace Numbers With Text Representation.
#'
#' A wrapper for \code{\link[textclean]{replace_number}} Replaces numeric represented numbers with words (e.g., 1001 becomes one thousand one).
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceNumbers$new(x)$execute()
#' @usage ReplaceNumbers$new(x, joinNumbers = TRUE, remove = FALSE)$execute()
#'
#' @template textCleanParams
#' @param joinNumbers Logical. If FALSE the elements of larger numbers are separated with spaces. If TRUE the elements will be joined without spaces.
#' @param remove Logical. If TRUE numbers are removed from the text.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceNumbers} Returns a vector with numbers replaced or removed.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceNumbers <- R6::R6Class(
  classname = "ReplaceNumbers",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(

    ..joinNumbers = logical(),
    ..remove = logical(),

    processText = function(content) {
      content <- textclean::replace_number(x = content,
                                            num.paste = private$..joinNumbers,
                                            remove = private$..remove)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, joinNumbers = FALSE, remove = FALSE) {
      private$..className <- "ReplaceNumbers"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceNumbers"
      private$..x <- x
      private$..joinNumbers <- joinNumbers
      private$..remove <- remove
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
