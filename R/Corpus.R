#==============================================================================#
#                                 Corpus                                       #
#==============================================================================#
#' Corpus
#'
#' \code{Corpus} Class that defines a corpus or collection of documents
#'
#' Class contains a collection of text documents along with document
#' transformations such as NGrams, and POS tagged documents.
#'
#' @section Corpus Core Methods:
#'  \describe{
#'   \item{\code{new(name, desc = NULL, studio = NULL)}}{Creates an object of Corpus Class}
#'   \item{\code{desc}}{A getter/corpuster method allowing clients to retrieve and corpus the Corpus description variable.}
#'   \item{\code{parent}}{A getter/corpuster method allowing clients to retrieve and corpus the Studio object to which the Corpus object belongs.}
#'   \item{\code{getName()}}{Returns the name of the Corpus object.}
#'   \item{\code{getPath()}}{Returns the path of the Corpus object.}
#'   \item{\code{getDocuments()}}{Returns the list of Document class objects.}
#'   \item{\code{addDocument(document)}}{Adds a Document object to the corpus.}
#'   \item{\code{removeDocument(document)}}{Removes a Document object from the corpus.}
#'   \item{\code{logIt(level = 'Info', fieldName = NA)}}{Formats the log and calls the LogR class to log an event.}
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#'  }
#'
#' @param name A character string containing the name of the Corpus object. This variable is used in the instantiation and remove methods.
#' @param desc A chararacter string containing the description of the Corpus.
#' @param document An object of one of the Document sub-classes.
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Corpus <- R6::R6Class(
  classname = "Corpus",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..documents = list(),

    getFiles = function() {

      if (isDirectory(private$..path)) {
        files <- list.files(private$..path, full.names = TRUE)
      } else {
        dirName <- dirname(private$..path)
        wildcard <- basename(private$..path)
        files <- list.files(dirName, path = glob2rx(wildcard), full.names = TRUE)
      }
      return(files)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Corpus Instantiation                            #
    #-------------------------------------------------------------------------#
    initialize = function(pattern = NULL) {

      # Instantiate variables
      private$..className <- 'Corpus'
      private$..methodName <- 'initialize'
      private$..parent <- NULL
      private$..state <- "Corpus instantiated."
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..accessed <- Sys.time()
      private$..logs <- LogR$new()

      # Validate Corpus
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

    #-------------------------------------------------------------------------#
    #                         Document Creation                               #
    #-------------------------------------------------------------------------#
    createDocuments = function() { stop("This method is not implemented for the abstract class.") },

    #-------------------------------------------------------------------------#
    #                         Composite Methods                               #
    #-------------------------------------------------------------------------#
    getDocuments = function() private$..documents,

    addDocument = function(document) {

      # Update current method
      private$..methodName <- 'addDocument'

      # Validation
      v <- Validator$new()
      status <- v$addChild(self, document)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Get document information
      documentName <- document$getName()

      # Add document to list of documents
      private$..documents[[documentName]] <- document

      # Set parent reference on document
      document$setParent(self)

      # Update modified time
      private$..accessed <- Sys.time()
      private$..modified <- Sys.time()

      # Save state and log Event
      private$..state <-
        paste("Document", documentName, "added to corpus,", private$..name, "at", Sys.time())
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },

    removeDocument = function(document) {

      # Update current method
      private$..methodName <- 'removeDocument'

      # Validation
      v <- Validator$new()
      status <- v$removeChild(self, document)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Obtain collection information
      documentName <- document$getName()

      # Remove collection from studio and update modified time
      private$..documents[[documentName]] <- NULL

      # Set parent of document to NULL
      document$setParent(NULL)

      # Update modified time
      private$..accessed <- Sys.time()
      private$..modified <- Sys.time()

      # Save state and log Event
      private$..state <-
        paste("Document", documentName, "removed from ", private$..name, "at", Sys.time())
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                              IO Methods                                 #
    #-------------------------------------------------------------------------#
    read = function() {
      private$..documents <- lapply(private$..documents, function(d) {
        d$read()
      })
    },

    write = function() {
      private$..documents <- lapply(private$..documents, function(d) {
        d$write()
      })
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$corpus(self)
    },

    #-------------------------------------------------------------------------#
    #                           Test Methods                                  #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      #TODO: Remove after testing

      corpus = list(
        className = private$..className,
        methodName = private$..methodName,
        name = private$..name,
        desc = private$..desc,
        path = private$..path,
        parent = private$..parent,
        documents = private$..documents,
        logs = private$..logs,
        state = private$..state,
        modified = private$..modified,
        created = private$..created
      )

      return(corpus)
    }
  )
)
