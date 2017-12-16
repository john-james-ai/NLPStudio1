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
#'  }
#'
#' @section IO Methods:
#'  \describe{
#'   \item{\code{read(io = NULL)}}{Reads a corpus into the Corpus object.}
#'   \item{\code{write(io = NULL)}}{Writes a Corpus object to file.}
#'  }
#'
#' @section Composite Methods:
#'  \describe{
#'   \item{\code{getDocuments()}}{Returns the list of Document class objects.}
#'   \item{\code{addDocument(document)}}{Adds a Document object to the corpus.}
#'   \item{\code{removeDocument(document)}}{Removes a Document object from the corpus.}
#'  }
#'
#' @section Analysis Methods:
#'  \describe{
#'   \item{\code{stats()}}{Produces a data frame with basic descriptive statistics for the corpus. }
#'   \item{\code{diversity}}{Produces a data frame of lexical diversity measures for the corpus.}
#'   \item{\code{readability}}{Produces a data frame of readability measures for the corpus.}
#'  }
#'
#' @section Meta Data Methods:
#'  \describe{
#'   \item{\code{docMeta(field)}}{Creates a document meta data field.}
#'   \item{\code{corpusMeta(field)}}{Creates a corpus meta data field.}
#'  }
#'
#' @section Other Methods:
#'  \describe{
#'   \item{\code{logIt(level = 'Info', fieldName = NA)}}{Formats the log and calls the LogR class to log an event.}
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#'  }
#'
#'
#' @param name A character string containing the name of the Corpus object. This variable is used in the instantiation and remove methods.
#' @param desc A chararacter string containing the description of the Corpus.
#' @param document An object of one of the Document sub-classes.
#' @param field Character string name for a field to be added to the Document or Corpus object meta data.
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

      if (isDirectory(private$..pattern)) {
        files <- list.files(private$..pattern, full.names = TRUE)
      } else {
        dirName <- dirname(private$..pattern)
        wildcard <- basename(private$..pattern)
        files <- list.files(dirName, pattern = glob2rx(wildcard), full.names = TRUE)
      }
      return(files)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Corpus Instantiation                            #
    #-------------------------------------------------------------------------#
    initialize = function(name, pattern = NULL) {

      # Instantiate variables
      private$..className <- 'Corpus'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..parent <- NULL
      private$..pattern <- pattern
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

    getPattern = function() private$..pattern,

    #-------------------------------------------------------------------------#
    #                         Document Creation                               #
    #-------------------------------------------------------------------------#
    build = function() {

      # Validate Corpus prior to build
      v <- Validator$new()
      status <- v$build(self)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      files <- private$getFiles()

      lapply(files, function(f) {
        name <- tools::file_path_sans_ext(basename(f))
        doc <- Document$new(name = name, path = f)
        self$addDocument(doc)
      })

      private$..state <- paste('Corpus built,', length(private$..documents),
                               'documents added.')
      private$..modified <- Sys.time()
      private$..accessed <- Sys.time()
      self$logIt()

      invisible(self)
    },

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
    #                           Analysis Methods                              #
    #-------------------------------------------------------------------------#
    stats = function() {
      a <- Analyzer$new()
      analysis <- a$stats(self)
      return(analysis)
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
        pattern = private$..pattern,
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
