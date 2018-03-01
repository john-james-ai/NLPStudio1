#==============================================================================#
#                               Text                                           #
#==============================================================================#
#' Text
#'
#' \code{Text} Class containing Text objects attached to Document objects.
#'
#' Defines a text object in terms of meta data and content.
#'
#' @section Text methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the Text class.}
#'  }
#'  
#' @param id Character string identifier to be assigned to the Text object.
#' @param content Character vectors containing actual content of the Text object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family TextStudio classes
#' @export
Text <- R6::R6Class(
  classname = "Text",
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
        private$..state <- "Updated Text object content."
        self$logIt()
        invisible(self)
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(content) {

      # Instantiate variables
      private$..className <- 'Text'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()
      private$..meta[["id"]] <- private$createId()
      private$..meta[['type']] <- class(self)[1]
      private$..meta[['user']] <- Sys.info()["user"]
      private$..meta[["created"]] <- Sys.time()
      private$..meta[["modified"]] <- Sys.time()
      private$..meta[["accessed"]] <- Sys.time()
      private$..content <- content

      # Create log entry
      private$..state <- paste0("Text object id ", private$..meta[["id"]], ", instantiated.")
      self$logIt()

      invisible(self)
    },
    
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$text(self)
    }
  )
)
