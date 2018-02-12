#------------------------------------------------------------------------------#
#                             Replace NonAscii                                 #
#------------------------------------------------------------------------------#
#' ReplaceNonAscii
#'
#' \code{ReplaceNonAscii}  Replace Common Non-ASCII Characters.
#'
#' A wrapper for \code{\link[textclean]{replace_non_ascii}}
#' Replaces common non-ascii characters.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceNonAscii$new(x)$execute()
#' @usage ReplaceNonAscii$new(x, removeNonConverted = FALSE)$execute()
#'
#' @template textCleanParams
#' @param removeNonConverted Logical. If TRUE unmapped encodings are deleted from the string.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceNonAscii} Returns a vector with non-ascii characters replaced
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceNonAscii <- R6::R6Class(
  classname = "ReplaceNonAscii",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(

    ..removeNonConverted = logical(),

    processText = function(content) {
      content <- textclean::replace_non_ascii(x = content,
                                              remove.nonconverted = private$..removeNonConverted)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, removeNonConverted = TRUE) {
      private$..className <- "ReplaceNonAscii"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceNonAscii"
      private$..x <- x
      private$..names <- names
      private$..removeNonConverted <- removeNonConverted
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
