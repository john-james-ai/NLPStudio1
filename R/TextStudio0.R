#==============================================================================#
#                               TextStudio0                                    #
#==============================================================================#
#' TextStudio0
#'
#' \code{TextStudio0} Abstract class  for the TextStudio family of classes.
#'
#' This abstract class defines a common interface and methods for the TextStudio
#' family of classes.
#'
#' @template textStudioParams
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio classes
#' @export
TextStudio0 <- R6::R6Class(
  classname = "TextStudio0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..x = character(),
    ..regex = character(),
    ..replacement = character(),
    
    processDocument = function(document) {
      content <- gsub(private$..regex,
                      private$..replacement,
                      document$text, perl = TRUE)
      document$text <- content
      return(document)
    },
    
    processCorpus = function() {
      docs <- private$..x$getDocuments()
      for (i in 1:length(docs)) {
        doc <- private$processDocument(docs[[i]])
        private$..x$attach(doc)
      }
      return()
    }
  ),

  public = list(
    initialize = function(x, ...) { stop("Not implemented for this abstract/interface class.") },

    execute = function() {

      private$..methodName <- "execute"

      if ("Corpus" %in% class(private$..x)) {
        private$processCorpus()
        
      } else {
        private$processDocument(private$..x)
      } 

      # Log it
      private$..state <- paste0("Executed ", class(self)[1], " on ",
                                private$..x$getName(), ". ")
      self$logIt()

      return(private$..x)
    }
  )
)