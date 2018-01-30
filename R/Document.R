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
    ..documentCache = character()
  ),

  active = list(
    content = function(value) {

      if (missing(value)) {
        return(private$..documentCache$read(object = self))
      } else {
        private$..documentCache$write(object = self, content = value)
        private$..modified <- Sys.time()
        private$..accessed <- Sys.time()
      }
      invisible(self)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name) {

      # Instantiate variables
      private$..meta[["name"]] <- name
      private$..className <- 'Document'
      private$..methodName <- 'initialize'
      private$..state <- paste0("Document, ", private$..meta[["name"]], ", instantiated.")
      private$..logs <- LogR$new()
      private$..created <- Sys.time()
      private$..modified <- Sys.time()
      private$..accessed <- Sys.time()
      private$..id <- private$createId()
      private$..documentCache <- Cache$new()

      # Create log entry
      self$logIt()

      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                             IO Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(io = NULL) {

      private$..methodName <- 'read'
      private$..state <- paste0("Read ", private$..name, " from cache.")
      private$..accessed <- Sys.time()
      self$logIt()
      return(private$..documentCache$read(self, io))
    },

    write = function(content, io = NULL) {

      private$..methodName <- 'save'
      private$..documentCache$write(self, content, io)
      private$..modified <- Sys.time()
      private$..accessed <- Sys.time()
      private$..state <- paste0("Saved ", private$..name, " to cache. ")
      self$logIt()
      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$document(self)
    },

    #-------------------------------------------------------------------------#
    #                            Test Methods                                 #
    #-------------------------------------------------------------------------#
    exposeObject = function() {
      document <- list(
        id = private$..id,
        meta = self$meta(),
        state = private$..state,
        created = private$..created,
        modified = private$..modified,
        accessed = private$..accessed,
        documentCache = private$..documentCache
      )
      return(document)
    }
  )
)
