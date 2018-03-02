#==============================================================================#
#                               Text                                           #
#==============================================================================#
#' Text
#'
#' \code{Text} Class containing data objects attached to Document objects.
#'
#' Standard class for data objects, such as tokenized documents, POS tags,
#' nGrams and the like. This non client-facing class is only invoked by 
#' a TextStudio class when creating objects which are then attached to
#' objects of the Document class.
#'
#' @section Text methods:
#'  \itemize{
#'   \item{\code{new()}}{Initializes an object of the Text class.}
#'  }
#'  
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
  inherit = Attachment0,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(content) {

      # Instantiate variables
      private$..className <- 'Text'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()
      private$..id <- private$createId()
      private$..content <- content
      private$..meta[["attachId"]] <- class(self)[1]
      private$..meta[['class']] <- class(self)[1]
      private$..meta[['user']] <- Sys.info()["user"]
      private$..meta[["created"]] <- Sys.time()
      private$..meta[["modified"]] <- Sys.time()
      private$..meta[["accessed"]] <- Sys.time()

      # Create log entry
      private$..state <- paste0("Text, ", private$..meta[["name"]], ", instantiated.")
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
