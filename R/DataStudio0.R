#==============================================================================#
#                               DataStudio0                                    #
#==============================================================================#
#' DataStudio0
#'
#' \code{DataStudio0} Abstract class  for the DataStudio family of classes.
#'
#' This abstract class defines a common interface and methods for the DataStudio
#' family of classes.
#'
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family DataStudio classes
#' @export
DataStudio0 <- R6::R6Class(
  classname = "DataStudio0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..x = character(),
    ..result = character()
  ),

  public = list(
    initialize = function(x, ...) { stop("Not implemented for this abstract/interface class.") },

    execute = function() {

      private$..methodName <- "execute"
      
      # Initialize resulting corpus
      private$..result <- private$..x
      
      indocs <- private$..x$getDocuments()
      outdocs <- private$..result$getDocuments()
      
      for (i in 1:length(indocs)) {
        n <- indocs$getName()
        outdocs[[i]]$content <- private$processData(indocs[[i]]$content)
        private$..result$addDocument(outdocs[[i]])
      }

      # Log it
      private$..state <- paste0("Executed ", class(self)[1], " on ",
                                private$..x$getName(), ". ")
      self$logIt()

      return(private$..result)
    }
  )
)