#------------------------------------------------------------------------------#
#                        Replace Abbreviations                                 #
#------------------------------------------------------------------------------#
#' ReplaceAbbreviations
#'
#' \code{ReplaceAbbreviations} Replaces abbreviations with long form.
#'
#' This is a wrapper for the replace_abbreviations function in the QDAP package.
#' Source \url{https://cran.r-project.org/web/packages/qdap/qdap.pdf}
#'
#' @usage ReplaceAbbreviations$new(x, replace, ignoreCase = TRUE)$execute()
#'
#' @template textCleanParams
#' @param abbreviation A two column key of abbreviations (column 1) and long form replacements (column 2) or a vector of abbreviations. Default is to use qdapDictionaries's abbreviations data set.
#' @param replacement Vector of long form replacements if a data frame is not supplied to the abbreviation argument.
#' @param ignoreCase Logical. If TRUE replaces without regard to capitalization.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceAbbreviations <- R6::R6Class(
  classname = "ReplaceAbbreviations",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(
    ..abbreviation = character(),
    ..replacement = character(),
    ..ignoreCase = character(),

    processText = function(content) {
      content <- qdap::replace_abbreviation(content,
                                            private$..abbreviation,
                                            private$..replacement,
                                            private$..ignoreCase)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, abbreviation = NULL,
                          replacement = NULL, ignoreCase = TRUE) {
      private$..className <- "ReplaceAbbreviations"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceAbbreviations"
      private$..x <- x
      private$..abbreviation <- abbreviation
      private$..replacement <- replacement
      private$..ignoreCase <- ignoreCase
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
