#------------------------------------------------------------------------------#
#                             Replace Kern                                     #
#------------------------------------------------------------------------------#
#' ReplaceKern
#'
#' \code{ReplaceKern}  Replace Kern
#'
#' A wrapper for \code{\link[textclean]{replace_kern}}
#' replaces kern. In typography kerning is the adjustment of spacing. Often,
#' in informal writing, adding manual spaces (a form of kerning) coupled
#' with all capital letters is used for emphasis. This tool looks for 3 or
#' more consecutive capital letters with spaces in between and removes the spaces.
#' Essentially, the capitalized, kerned version is replaced with the word equivalent.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceKern$new(x)$execute()
#'
#' @template textCleanParams
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceKern} Returns a vector with kern replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceKern <- R6::R6Class(
  classname = "ReplaceKern",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Text0,

  private = list(

    processText = function(content) {
      content <- textclean::replace_kern(x = content)
      return(content)
    }
  ),

  public = list(
    initialize = function(x) {
      private$..className <- "ReplaceKern"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceKern"
      private$..x <- x
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
