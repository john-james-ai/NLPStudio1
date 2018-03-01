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
#' @template textStudioParams
#' @param abbreviations A two column key of abbreviations (column 1) and long form replacements (column 2) or a vector of abbreviations. Default is to use qdapDictionaries's abbreviations data set.
#' @param replacement Vector of long form replacements if a data frame is not supplied to the abbreviation argument.
#' @param ignoreCase Should case be ignored? Only applies to default dictionary.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceAbbreviations <- R6::R6Class(
  classname = "ReplaceAbbreviations",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(
    ..abbreviations = character(),
    ..ignoreCase = logical(),

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
          }
        } else {
          pattern <- private$..abbreviations
          replacement <- private$..replacement
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
      
      if (private$validateParams()$code == FALSE) stop()
      
      invisible(self)
    },
    
    getParams = function() {
      input <- list(
        x = private$..x,
        pattern = private$..abbreviations,
        replacement = private$..replacement,
        ignoreCase = private$..ignoreCase
      )
      return(input)
    },
    accept = function(visitor)  {
      visitor$replaceAbbreviations(self)
    }
  )
)
