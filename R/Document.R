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
#'   \item{\code{getContent()}}{Method that returns the content of the Document object. }
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
  inherit = Document0,

  private = list(
    ..content = list()
  ),

  active = list(
    content = function(value) {

      if (missing(value)) {
        private$..meta[["accessed"]] <- Sys.time()
        return(private$..content)
      } else {
        if (is.null(private$..content)) private$..meta[["created"]] <- Sys.time()
        private$..content <- value
        private$..meta[["modified"]] <- Sys.time()
        private$..meta[["accessed"]] <- Sys.time()
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
      private$..meta[['name']] <- name
      private$..state <- paste0("Document, ", private$..meta[["name"]], ", instantiated.")
      private$..logs <- LogR$new()
      private$..meta[["created"]] <- Sys.time()
      private$..meta[["modified"]] <- Sys.time()
      private$..meta[["accessed"]] <- Sys.time()

      # Create log entry
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Content Methods                             #
    #-------------------------------------------------------------------------#
    setContent = function(content) {

      private$..content <- content
      private$..meta[["modified"]] <- Sys.time()
      private$..meta[["accessed"]] <- Sys.time()

      invisible(self)
    },

    getContent = function() {
      private$..meta[["accessed"]] <- Sys.time()
      private$..content
    },


    #-------------------------------------------------------------------------#
    #                             IO Methods                                  #
    #-------------------------------------------------------------------------#
    load = function(path, io = NULL) {

      private$..methodName <- 'load'

      # Validation Path
      status <- private$validatePath(path)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt("Error")
        stop()
      }

      # Validate/instantiate IO
      if (is.null(io)) {
        io <- IOFactory$new(path)$getIOStrategy()
      } else {
        status <- private$validateIO(io)
        if (status[['code']] == FALSE) {
          private$..state <- status[['msg']]
          self$logIt("Error")
          stop()
        }
      }

      # Read content
      private$..content <- io$read(path)

      # Format document file meta data
      private$..meta[["directory"]] <- dirname(path)
      private$..meta[["fileName"]] <- basename(path)
      private$..meta[["fileSize"]] <- file.size(path)
      private$..meta[["format"]] <- ifelse(class(private$..content) == 'raw', "bin", tools::file_ext(path))
      private$..meta[["created"]] <- file.info(path)[,'ctime']
      private$..meta[["modified"]] <- file.info(path)[,'mtime']
      private$..meta[["accessed"]] <- Sys.time()

      # LogIt
      private$..state <- paste0("Read ", private$..meta[["name"]], ". ")
      self$logIt()

      invisible(self)
    },

    save = function(path, io = NULL) {

      private$..methodName <- 'save'

      # Validation Path
      status <- private$validatePath(path)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt("Error")
        stop()
      }

      # Validate/instantiate IO
      if (is.null(io)) {
        io <- IOFactory$new(path)$getIOStrategy()
      } else {
        status <- private$validateIO(io)
        if (status[['code']] == FALSE) {
          private$..state <- status[['msg']]
          self$logIt("Error")
          stop()
        }
      }

      # Validate content
      if (is.null(private$..content)) {
        private$..state <- paste0("Unable to save document. Content is NULL. ",
                                  "See ?", class(self)[1], "for further assistance.")
        self$logIt("Error")
        stop()
      }

      # Write data
      io$write(path, private$..content)

      # Format document file meta data
      private$..meta[["directory"]] <- dirname(path)
      private$..meta[["fileName"]] <- basename(path)
      private$..meta[["fileSize"]] <- file.size(path)
      private$..meta[["format"]] <- ifelse(class(private$..content) == 'raw', "bin", tools::file_ext(path))
      private$..meta[["created"]] <- file.info(path)[,'ctime']
      private$..meta[["modified"]] <- file.info(path)[,'mtime']
      private$..meta[["accessed"]] <- Sys.time()

      # LogIt
      private$..state <- paste0("Wrote ", private$..meta[["name"]], ". ")
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
        name = private$..meta[["name"]],
        meta = self$meta(),
        content = private$..content,
        state = private$..state,
        created = private$..meta[["created"]],
        modified = private$..meta[["modified"]],
        accessed = private$..meta[["accessed"]],
        file = private$..file
      )
      return(document)
    }
  )
)
