#==============================================================================#
#                               Data                                           #
#==============================================================================#
#' Data
#'
#' \code{Data} Class containing data objects attached to Document objects.
#'
#' Standard class for data objects, such as tokenized documents, POS tags,
#' nGrams and the like. This non client-facing class is only invoked by 
#' a DataStudio class when creating objects which are then attached to
#' objects of the Document class.
#'
#' @section Data methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the Data class.}
#'   \item{\code{summarize()}}{Returns meta data for a Data object.}
#'  }
#'  
#' @param id Character string identifier to be assigned to the Data object.
#' @param content Character vectors containing actual content of the Data object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family DataStudio classes
#' @export
Data <- R6::R6Class(
  classname = "Data",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..content = character()
  ),

  active = list(
    content = function(value) {

      if (missing(value)) {
        private$..meta[['user']] <- Sys.info()["user"]
        private$..meta[["accessed"]] <- Sys.time()
        return(private$..content)
      } else {
        private$..content <- value
        private$..meta[['user']] <- Sys.info()["user"]
        private$..meta[["modified"]] <- Sys.time()
        private$..meta[["accessed"]] <- Sys.time()
        private$..state <- "Updated data object content."
        self$logIt()
        invisible(self)
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(id, content) {

      # Instantiate variables
      private$..meta[["id"]] <- id
      private$..className <- 'Data'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()
      private$..content <- content
      private$..meta[['type']] <- class(self)[1]
      private$..meta[['user']] <- Sys.info()["user"]
      private$..meta[["created"]] <- Sys.time()
      private$..meta[["modified"]] <- Sys.time()
      private$..meta[["accessed"]] <- Sys.time()

      # Create log entry
      private$..state <- paste0("Data, ", private$..meta[["name"]], ", instantiated.")
      self$logIt()

      invisible(self)
    },

    summarize = function() { private$..meta },
    
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$data(self)
    }
  )
)
