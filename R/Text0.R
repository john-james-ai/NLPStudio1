#==============================================================================#
#                               Text0                                     #
#==============================================================================#
#' Text0
#'
#' \code{Text0} Abstract class  for the TextClean family of classes.
#'
#' This abstract class defines a common interface and methods for the TextClean
#' family of classes.
#'
#' @template textCleanParams
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean classes
#' @export
Text0 <- R6::R6Class(
  classname = "Text0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..x = character(),
    ..regex = character(),
    ..replacement = character(),
    
    processText = function(content) {
      content <- gsub(private$..regex,
                      private$..replacement,
                      content, perl = TRUE)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, ...) { stop("Not implemented for this abstract/interface class.") },

    execute = function() {

      private$..methodName <- "execute"

      if ("Corpus" %in% class(private$..x)) {
        documents <- private$..x$getDocuments()
        for (i in 1:length(documents)) {
          documents[[i]]$content <- private$processText(documents[[i]]$content)
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