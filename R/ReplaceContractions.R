#------------------------------------------------------------------------------#
#                         Replace Contractions                                 #
#------------------------------------------------------------------------------#
#' ReplaceContractions
#'
#' \code{ReplaceContractions}  Replace contractions.
#'
#' A wrapper for \code{\link[textclean]{replace_contraction}} that
#' replaces contractions with long form.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceContractions$new(x, contractions = lexicon::key_contractions, ignoreCase = TRUE)$execute()
#'
#' @template textCleanParams
#' @param contractions Character string of contractions to be matched in the 
#' given character vector. If NULL, the default is to use \code{\link[lexicon]{key_contractions}} data set.
#' @param replacement Character string equal in length to contractions containing
#'  the long forms of the contractions. 
#' @param leadspace logical.  If \code{TRUE} inserts a leading space in the 
#' replacements.
#' @param trailspace logical.  If \code{TRUE} inserts a trailing space in the 
#' replacements.
#' @param fixed logical. If \code{TRUE}, contractions is a string to be matched as is. 
#' Overrides all conflicting arguments.
#' @param trim logical.  If \code{TRUE} leading and trailing white spaces are 
#' removed and multiple white spaces are reduced to a single white space.
#' @param orderPattern logical.  If \code{TRUE} and \code{fixed = TRUE}, the 
#' \code{contractions} string is sorted by number of characters to prevent substrings 
#' replacing meta strings (e.g., \code{contractions = c("the", "then")} resorts to 
#' search for "then" first).
#' @param \dots ignored.
#'
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceContractions} Returns a vector with contractions replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceContractions <- R6::R6Class(
  classname = "ReplaceContractions",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Text0,
  
  private = list(
    ..contractions = character(),
    ..leadspace = logical(),
    ..trailspace = logical(),
    ..fixed = logical(),
    ..trim = logical(),
    ..orderPattern = character(),
    
    processText = function(content) {
      if (is.null(private$..contractions)) {
        content <- textclean::replace_contraction(x = content,
                                                ignore.case = private$..ignoreCase)
      } else {
        content <- textclean::mgsub(x = content, 
                                    pattern = private$..contractions,
                                    replacement = private$..replacement,
                                    leadspace = private$..leadspace,
                                    trailspace = private$..trailspace,
                                    fixed = private$..fixed,
                                    trim = private$..trim,
                                    order.pattern = private$..orderPattern)
      }
      return(content)
    }
  ),
  
  public = list(
    initialize = function(x, contractions = NULL, replacement = NULL, leadspace = FALSE, 
                          trailspace = FALSE, fixed = TRUE, trim = FALSE, 
                          orderPattern = fixed) {
      private$..className <- "ReplaceContractions"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceContractions"
      private$..x <- x
      private$..contractions <- contractions
      private$..replacement <- replacement
      private$..leadspace <- leadspace
      private$..trailspace <- trailspace
      private$..fixed <- fixed
      private$..trim <- trim
      private$..orderPattern <- orderPattern
      private$..logs  <- LogR$new()
      
      if (private$validateParams()$code == FALSE) stop()
      
      invisible(self)
    },
    
    getParams = function() {
      input <- list(
        x = private$..x,
        pattern = private$..contractions,
        replacement = private$..replacement,
        leadspace = private$..leadspace,
        trailspace = private$..trailspace,
        fixed = private$..fixed,
        trim = private$..trim,
        orderPattern = private$..orderPattern
      )
      return(input)
    },
    accept = function(visitor)  {
      visitor$replaceContractions(self)
    }
  )
)