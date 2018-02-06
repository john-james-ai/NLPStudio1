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
    ..content = character()
  ),

  active = list(
    content = function(value) {

      if (missing(value)) {
        return(private$..content)
      } else {
        private$..content <- value
        private$..modified <- Sys.time()
        private$..accessed <- Sys.time()
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
      private$..meta[["name"]] <- name
      private$..className <- 'Document'
      private$..methodName <- 'initialize'
      private$..state <- paste0("Document, ", private$..meta[["name"]], ", instantiated.")
      private$..logs <- LogR$new()
      private$..created <- Sys.time()
      private$..modified <- Sys.time()
      private$..accessed <- Sys.time()
      private$..id <- private$createId()

      # Create log entry
      self$logIt()

      invisible(self)
    },

    getFileName = function() {
      private$..methodName <- "getFileName"
      if (!is.null(private$..meta$fileName)) {
        return(private$..meta$fileName)
      } else {
        return(NULL)
      }
    },

    #-------------------------------------------------------------------------#
    #                             IO Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(path = NULL) {

      private$..methodName <- 'read'

      if (!is.null(path)) {
        if (file.exists(path)) {
          created <- file.ctime(path)
          modified <- file.mtime(path)
          if (private$..accessed <- created | private$..accessed <- modified) {
            io <- IOFactory$new(path)$getIOStrategy()
            private$..content <- io$read(path)
            private$..state <- paste0("Read ", private$..meta["name"], " from ", path, ".")
          }
        } else {
          private$..state <- paste0("Unable to read document from file.  Path ",
                                    path, " does not exist. For further assistance, ",
                                    "See ?", class(self)[1], ". ")
          self$logIt("Error")
          stop()
        }
      }

      private$..accessed <- Sys.time()
      self$logIt()
      return(private$..content)
    },

    write = function(path, io = NULL) {

      private$..methodName <- 'write'

      if (is.null(io)) io <- IOText$new()

      io$write(path = path, content = private$..content)

      private$..state <- paste0("Saved ", private$..meta["name"], " to ", path, ". ")
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
        accessed = private$..accessed
      )
      return(document)
    }
  )
)
