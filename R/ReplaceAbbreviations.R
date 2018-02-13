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
#' @param abbreviations A two column key of abbreviations (column 1) and long form replacements (column 2) or a vector of abbreviations. Default is to use qdapDictionaries's abbreviations data set.
#' @param replacement Vector of long form replacements if a data frame is not supplied to the abbreviation argument.
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
    ..abbreviations = character(),
    ..ignoreCase = character(),

    processText = function(content) {
      if (is.null(private$..abbreviations)) {
        content <- qdap::replace_abbreviation(text.var = content, 
                                              ignore.case = private$..ignoreCase)
        
      } else {
        if ("data.frame" %in% class(private$..abbreviations)) {
          pattern <- as.character(private$..abbreviations[,1])
          if (ncol(private$..abbreviations) == 2) {
            replacement <- as.character(private$..abbreviations[,2])
          } else if (ncol(private$..abbreviations) == 1) {
            replacement <- as.character(private$..replacement)
            if (length(pattern) != length(replacement)) {
              private$..state <- "Abbreviations and replacement vectors must be of equal length."
              self$logIt("Error")
              stop()
            }
          }
        } else {
          pattern <- private$..abbreviations
          replacement <- private$..replacement
          if (length(pattern) != length(replacement)) {
            private$..state <- "Abbreviations and replacement vectors must be of equal length."
            self$logIt("Error")
            stop()
          }
        }
          
        content <- textclean::mgsub(x = content,  pattern = pattern,
                                    replacement = replacement,
                                    fixed = TRUE)
      }
      return(content)
    }
  ),

  public = list(
    initialize = function(x, abbreviations = NULL,
                          replacement = NULL, ignoreCase = TRUE) {
      private$..className <- "ReplaceAbbreviations"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceAbbreviations"
      private$..x <- x
      private$..abbreviations <- abbreviations
      private$..replacement <- replacement
      private$..ignoreCase <- ignoreCase
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
