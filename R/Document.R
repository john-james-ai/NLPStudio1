#==============================================================================#
#                               Document                                       #
#==============================================================================#
#' Document
#'
#' \code{Document} Class containing the data and methods for corpus documents
#'
#' Defines the object and behavior of the Document object, a composite of
#' the Corpus class.
#'
#' @section Document core methods:
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a document. Not implemented for the abstract class.}
#'   \item{\code{getName()}}{Method that returns the name of the Document object. }
#'   \item{\code{getFileName()}}{Method for obtaining the document file name. .}
#'   \item{\code{getPath()}}{Method for obtaining the document path. }
#'  }
#'
#' @section Document getter/setter methods:
#'  \itemize{
#'   \item{\code{desc()}}{Method for setting or retrieving the Document object description.}
#'  }
#'
#'  @section Document IO methods:
#'  \itemize{
#'   \item{\code{getContent()}}{Method for obtaining the document content.}
#'   \item{\code{addContent(content)}}{Method for adding content to a document.}
#'   \item{\code{read(io)}}{Method for reading a document. }
#'   \item{\code{write(io)}}{Method for writing a document. }
#'  }
#'
#' @section Document aggregation method:
#'  \itemize{
#'   \item{\code{setParent(parent)}}{Sets the parent Corpus object. }
#'  }
#'
#'
#' @section Other methods:
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Accepts a visitor object. Not implemented for this abstract class}
#'   \item{\code{logIt(level = 'Info')}}{Logs events relating to the document.}
#'  }
#'
#'
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
  inherit = Document0,

  private = list(
    ..content = list()
  ),

  active = list(
    content = function(value) {

      if (missing(value)) {
        private$..accessed <- Sys.time()
        return(private$..content)
      } else {
        if (is.null(private$..content)) private$..created <- Sys.time()
        private$..content <- value
        private$..modified <- Sys.time()
        private$..accessed <- Sys.time()
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
      private$..name <- name
      private$..state <- paste0("Document, ", private$..name, ", instantiated.")
      private$..logs <- LogR$new()
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..accessed <- Sys.time()

      # Create log entry
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Content Methods                             #
    #-------------------------------------------------------------------------#
    setContent = function(content) {

      private$..content <- content
      private$..modified <- Sys.time()
      private$..accessed <- Sys.time()

      invisible(self)
    },

    getContent = function() {
      private$..accessed <- Sys.time()
      private$..content
    },


    #-------------------------------------------------------------------------#
    #                             IO Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(path, io = NULL) {

      private$..methodName <- 'read'

      # Validation
      private$..path <- private$validatePath(path)
      io <- private$validateIO(io)

      # Read content
      private$..content <- io$read(path)

      # Update file meta data
      private$..fileSize <- file.size(path)
      private$..format <- ifelse(class(private$..content) == 'raw', "bin", tools::file_ext(path))
      private$..created <- file.info(path)[,'ctime']
      private$..modified <- file.info(path)[,'mtime']
      private$..accessed <- Sys.time()

      # LogIt
      private$..state <- paste0("Read ", private$..name, ". ")
      self$logIt()

      return(private$..content)
    },

    write = function(path, io = NULL, content = NULL) {

      private$..methodName <- 'write'

      # Validation
      private$..path <- path
      io <- private$validateIO(io)
      private$..content <- private$validateContent(content)

      # Write data
      io$write(path, private$..content)

      # Update file meta data
      private$..fileSize <- file.size(path)
      private$..format <- ifelse(class(private$..content) == 'raw', "bin", tools::file_ext(path))
      private$..created <- file.info(path)[,'ctime']
      private$..modified <- file.info(path)[,'mtime']
      private$..accessed <- Sys.time()

      # LogIt
      private$..state <- paste0("Wrote ", private$..name, ". ")
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
        name = private$..name,
        meta = self$meta(),
        content = private$..content,
        state = private$..state,
        created = private$..created,
        modified = private$..modified,
        accessed = private$..accessed,
        file = private$..file
      )
      return(document)
    }
  )
)
