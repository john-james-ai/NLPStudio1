#------------------------------------------------------------------------------#
#                                  Tokenize                                    #
#------------------------------------------------------------------------------#
#' Tokenize
#'
#' \code{Tokenize}  Tokenizes text into character, word, or sentence tokens. 
#' 
#' This class is a wrapper for the  \code{\link[quanteda]{tokens}} function for
#' character, and word, tokenization. Sentence tokenization functionality is
#' provided using the openNLP package.
#' 
#' Sources: 
#' \url{https://cran.r-project.org/web/packages/quanteda/quanteda.pdf}
#' \url{https://cran.r-project.org/web/packages/openNLP/openNLP.pdf}
#'
#' @usage Tokenize$new(x, what = c("sentence", "word"))$execute()
#'
#' @template textStudioParams
#' @param what Character string containing either c('character', 'word' ,'sentence)
#' indicating to which format the document should be tokenized.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{Tokenize} A tokenized Corpus, Document, or character text
#' object.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
Tokenize <- R6::R6Class(
  classname = "Tokenize",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = DataStudio0,
  
  private = list(
    ..what = character(),
    
    processData = function(content) {
      if (private$..what == "sentence") {
        
        # Use sentence token from openNLP and NLP packages
        s <- paste(content, collapse = "")
        s <- NLP::as.String(s)
        sa <- openNLP::Maxent_Sent_Token_Annotator()
        a <- NLP::annotate(s, sa)
        tokenized <- s[a]

      } else {
        tokenized <- quanteda::tokens(x = content, what = private$..what)
      }
      return(tokenized)
    }
  ),
  
  public = list(
    initialize = function(x, what) {
      private$..className <- "Tokenize"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "Tokenize"
      private$..x <- x
      private$..what <- what
      private$..logs  <- LogR$new()
      
      if (private$validateParams()$code == FALSE) stop()
      
      invisible(self)
    },
    
    getParams = function() {
      input <- list(
        x = private$..x,
        what = private$..what
      )
      return(input)
    },
    accept = function(visitor)  {
      visitor$tokenize(self)
    }
  )
)