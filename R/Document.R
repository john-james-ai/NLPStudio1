#==============================================================================#
#                               Document                                       #
#==============================================================================#
#' Document
#'
#' \code{Document} Class containing the data and methods for corpus documents
#'
#' Defines the object and behavior of the Document object, a leaf component
#' of Corpus composite class.
#'
#' @template documentClasses
#'
#' @section Document methods:
#' \strong{Core Methods:}
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a document. Not implemented for the abstract class.}
#'   \item{\code{getName()}}{Method that returns the name of the Document object. }
#'   \item{\code{setContent(content)}}{Method that sets the content of the Document object. }
#'   \item{\code{read()}}{Method that returns the content of the Document object. }
#'  }
#'
#' \strong{IO Methods:}
#'  \itemize{
#'   \item{\code{read(path, io = NULL)}}{Reads document content from a source designated by the path parameter. }
#'   \item{\code{write(path, io = NULL, content = NULL)}}{Method for writing a document. }
#'  }
#'
#' \strong{Other Methods:}
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Accepts a visitor object. Not implemented for this abstract class}
#'   \item{\code{logIt(level = 'Info')}}{Logs events relating to the document.}
#'  }
#'
#' @section Parameters:
#' @param name Character string indicating the file path for a document
#' @param path Character string indicating the path to the docment file.
#' @param parent Corpus object to which the document is composed.
#' @param content List containing character vectors of text.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document classes
#' @export
Document <- R6::R6Class(
  classname = "Document",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..attachments = list()
    
  ),

  active = list(
    text = function(value) {

      if (missing(value)) {
        private$..text$meta(key = 'user', value = Sys.info()["user"])
        private$..text$meta(key = 'accessed', value = Sys.time())
        return(private$..text)
      } else {
        private$..text$content <- value
        private$..text$meta(key = 'user', value = Sys.info()["user"])
        private$..text$meta(key = 'modified', value = Sys.time())
        private$..text$meta(key = 'accessed', value = Sys.time())
        private$..state <- "Updated text content."
        self$logIt()
        invisible(self)
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name) {

      # Instantiate variables
      private$..className <- 'Document'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()
      private$..id <- private$createId()
      private$..meta[["attachId"]] <- paste0(class(self)[1], "-", name)
      private$..meta[['class']] <- class(self)[1]
      private$..meta[['name']] <- name
      private$..meta[['user']] <- Sys.info()["user"]
      private$..meta[["created"]] <- Sys.time()
      private$..meta[["modified"]] <- Sys.time()
      private$..meta[["accessed"]] <- Sys.time()

      if (private$validateParams()$code == FALSE) stop()

      # Create log entry
      private$..state <- paste0("Document ", name, ", instantiated.")
      self$logIt()

      invisible(self)
    },
    
    attach = function(object) {
      private$..methodName <- "attach"
      
      # Validate
      if (sum(class(object) %in% c("Text", "Data", "Analysis")) == 0) {
        private$..state <- paste0("Unable to attach object of class ", class(object)[1],
                                  ". Only objects of the Text, Data, and Analysis ",
                                  "classes may be attached to Document objects. ",
                                  "See ?", class(self)[1], " for further assistance.")
        self$logIt("Error")
        stop()
      }
      
      # Attach 
      aid <- object$attachId()
      private$..attachments[[aid]] <- object
      
      # Log
      private$..state <- paste0("Attached ", aid, " to ", private$..meta[["name"]], ".")
      self$logIt()
      
      invisible(self)
      
    },
    
    
    unattach = function(object) {
      private$..methodName <- "attach"
      
      # Validate
      aid <- object$attachId()
      
      if (exists(private$..attachments[[aid]])) {
        private$..attachments[[aid]] <- NULL
        private$..state <- paste0("Unattached ", aid, " from ", private$..meta[["name"]], ".")
        self$logIt()
      } else {
        private$..state <- paste0("Object ", aid, " was not attached to ", private$..meta[["name"]], ".")
        self$logIt("Warn")
      }
      
      invisible(self)
    },
    
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$document(self)
    }
  )
)
