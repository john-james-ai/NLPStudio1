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
  inherit = Meta,

  private = list(
    ..content = list(),
    ..file = character()
  ),

  active = list(
    content = function(value = NULL) {

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
    initialize = function(name, file = NULL) {

      # Instantiate variables
      private$..className <- 'Document'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..state <- paste0("Document, ", private$..name, ", instantiated.")
      private$..logs <- LogR$new()
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..accessed <- Sys.time()

      # Attach File object and contents
      private$..file <- file
      if (!is.null(file)) {
        private$..content <- file$read()
        self$docMeta(key = 'title', value = file$getName())
        file$flush()
      }

      # Validate Document
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Create log entry
      self$logIt()

      invisible(self)
    },

    getName = function() private$..name,
    getFile = function() private$..file,


    #-------------------------------------------------------------------------#
    #                             IO Methods                                  #
    #-------------------------------------------------------------------------#
    read = function() {

      private$..methodName <- 'read'

      if (private$..file$fileInfo()$mtime < private$..modified &
          !is.null(private$..content)) {
        content <- private$..content
      } else {
        content <- private$..file$read()
      }

      # LogIt
      private$..state <- paste0("Read ", private$..name, ". ")
      private$..accessed <- Sys.time()
      self$logIt()

      return(content)
    },

    write = function() {

      private$..methodName <- 'write'

      private$..file$content <- private$..content
      private$..file$write()

      # LogIt
      private$..state <- paste0("Wrote ", private$..name, ". ")
      private$..accessed <- Sys.time()
      self$logIt()

      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                         Meta Data Methods                               #
    #-------------------------------------------------------------------------#
    docMeta = function(key = NULL, value = NULL) {

      private$..methodName <- 'meta'

      # If no parameters, return meta data if available, else the metadata names
      if (is.null(key) & is.null(value)) {
        meta <- Filter(Negate(is.null), private$..meta$document)
        if (length(meta) == 0) {
          return(names(private$..meta$document))
        } else {
          return(as.data.frame(meta))
        }
      }

      if (is.null(key)) {
        key <- names(value)
      }

      if (is.null(key)) {
        key <- paste("docMeta", seq_len(ncol(as.data.frame(value))),
                     sep = "")
      }

      private$..meta$document[[key]] <- value
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
