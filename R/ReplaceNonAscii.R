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
#' @template textStudioParams
#' @param removeNonConverted Logical. If TRUE unmapped encodings are deleted from the string.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{ReplaceNonAscii} Returns a vector with non-ascii characters replaced
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceNonAscii <- R6::R6Class(
  classname = "ReplaceNonAscii",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Text0,

  private = list(

    ..removeNonConverted = logical(),

    processText = function(content) {
      content <- textclean::replace_non_ascii(x = content,
                                              remove.nonconverted = private$..removeNonConverted)
      content <- textclean::mgsub(x = content,
                                  pattern = NLPStudio:::encodings$pattern,
                                  replacement = NLPStudio:::encodings$replace)
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
