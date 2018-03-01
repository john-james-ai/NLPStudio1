#==============================================================================#
#                               Analysis0                                      #
#==============================================================================#
#' Analysis0
#'
#' \code{Analysis0} Abstract class for the Analysis family of classes.
#'
#' This abstract class defines a common interface and methods for the Analysis
#' family of classes.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family Analysis classes
#' @export
Analysis0 <- R6::R6Class(
  classname = "Analysis0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..analyses = list()
  ),

  public = list(
    initialize = function(x, ...) { stop("Not implemented for this abstract/interface class.") },

    analyze = function() {

      private$..methodName <- "analyze"

      if ("Corpus" %in% class(private$..x)) {
        documents <- private$..x$getDocuments()
        for (i in 1:length(documents)) {
          documents[[i]]$content <- private$analyzeContent(documents[[i]]$content)
          private$..x$addDocument(documents[[i]])
        }
      } else if ("Document" %in% class(private$..x)) {
        private$..x$content <- private$processText(private$..x$content)
      } else if ("list" %in% class(private$..x)) {
        for (i in 1:length(private$..x)) {
          private$..x[[i]] <- private$processText(private$..x[[i]])
        }
      } else {
        private$..x <- private$processText(private$..x)
      }

      # Log it
      private$..state <- paste0("Executed ", class(self)[1], " on ",
                                private$..x$getName(), ". ")
      self$logIt()

      return(private$..x)
    }
  )
)