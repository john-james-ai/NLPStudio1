#------------------------------------------------------------------------------#
#                              Add Missing Endmark                             #
#------------------------------------------------------------------------------#
#' AddEndMark
#'
#' \code{AddEndMark} Add missing endmarks.
#'
#' A wrapper for \code{\link[textclean]{add_missing_endmark}}, this class
#' detects missing endmarks and replaces them with the desired symbol.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage AddEndMark$new(x, replacement = "|", endmarks = c("?", ".", "!"), ...)$execute()
#'
#' @template textCleanParams
#' @param replacement Symbol added for missing endmarks
#' @param endmarks List of endmark symbols to detect
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
AddEndMark <- R6::R6Class(
  classname = "AddEndMark",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Text0,

  private = list(
    ..endmarks = character(),

    processText = function(content) {
      content <- textclean::add_missing_endmark(x = content,
                                                replacement = private$..replacement,
                                                endmarks = private$..endmarks)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, replacement = "|", endmarks = c("?", ".", "!"), ...) {

      private$..className <- "AddEndMark"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "AddEndMark"
      private$..x <- x
      private$..replacement <- replacement
      private$..endmarks <- endmarks
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
