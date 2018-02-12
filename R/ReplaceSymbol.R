#------------------------------------------------------------------------------#
#                             Replace Symbol                                   #
#------------------------------------------------------------------------------#
#' ReplaceSymbol
#'
#' \code{ReplaceSymbol}  Replace Symbols With Word Equivalents
#'
#' A wrapper for \code{\link[textclean]{replace_symbol}} This function replaces symbols
#' with word equivalents (e.g., @ becomes "at".
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceSymbol$new(x, dollar = TRUE)$execute()
#' @usage ReplaceSymbol$new(x, dollar = FALSE, percent = TRUE)$execute()
#'
#' @template textCleanParams
#' @param dollar logical. If TRUE replaces dollar sign (\$) with "dollar".
#' @param percent logical. If TRUE replaces percent sign (\%) with "percent".
#' @param pound logical. If TRUE replaces pound sign (\#) with "number".
#' @param at logical. If TRUE replaces at sign (\@) with "at".
#' @param and logical. If TRUE replaces and sign (\&) with "and".
#' @param  with logical. If TRUE replaces with sign (w/) with "with"
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceSymbol} Returns a vector with symbols replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceSymbol <- R6::R6Class(
  classname = "ReplaceSymbol",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(

    ..dollar = logical(),
    ..percent = logical(),
    ..pound = logical(),
    ..at = logical(),
    ..and = logical(),
    ..with = logical(),

    processText = function(content) {
      content <- textclean::replace_symbol(x = content,
                                            dollar = private$..dollar,
                                           percent = private$..percent,
                                           pound = private$..pound,
                                           at = private$..at,
                                           and = private$..and,
                                           with = private$..with)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, dollar = TRUE, percent = TRUE, pound = TRUE,
                          at = TRUE, and = TRUE, with = TRUE) {
      private$..className <- "ReplaceSymbol"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceSymbol"
      private$..x <- x
      private$..dollar <- dollar
      private$..percent <- percent
      private$..pound <- pound
      private$..at <- at
      private$..and <- and
      private$..with <- with
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
