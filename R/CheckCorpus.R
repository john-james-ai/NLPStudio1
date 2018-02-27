#------------------------------------------------------------------------------#
#                              CheckCorpus                                       #
#------------------------------------------------------------------------------#
#' CheckCorpus
#'
#' \code{CheckCorpus} Checks the text for potential errors and reports suggested courses of action
#' 
#' A wrapper for \code{\link[textclean]{check_text.R}} that checks text for 
#' potential anomalies, such as:  factors, missing ending punctuation, 
#' empty cells, double punctuation,  non-space after comma, no alphabetic 
#' characters, non-ASCII, missing value,  and potentially misspelled words.
#' emoticons with the words they represent.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage CheckCorpus$new(x)$execute()
#'
#' @template textStudioParams
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
CheckCorpus <- R6::R6Class(
  classname = "CheckCorpus",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Text0,
  
  private = list(
    ..file = character(),
    
    processText = function(content) {
      content <- textclean::check_text(x = content,
                                       file = private$..file)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, file = NULL) {
      private$..className <- "CheckCorpus"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "CheckCorpus"
      private$..x <- x
      private$..file <- file
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
