#==============================================================================#
#                                 Corpus                                       #
#==============================================================================#
#' Corpus
#'
#' \code{Corpus} Class of document collections or corpora.
#'
#' Class that contains document collections, or corpora, as well as the methods
#' for reading, writing, and managing the documents therein.
#'
#' @section Methods:
#' \strong{Core Methods}
#'  \describe{
#'   \item{\code{new(name, path, dataSource)}}{Instantiates an object of the Corpus class.}
#'  }
#'
#' \strong{Document Methods}
#'  \describe{
#'   \item{\code{getDocuments()}}{Returns the Corpus Document objects.}
#'   \item{\code{addDocument(document)}}{Adds a Document object to the Corpus.}
#'   \item{\code{removeDocument(document)}}{Removes a Document object from the Corpus.}
#'  }
#'
#' \strong{IO Methods}
#'  \describe{
#'   \item{\code{read(io = NULL)}}{Reads the corpus from file.}
#'   \item{\code{write(io = NULL)}}{Writes the corpus to file.}
#'  }
#'
#' \strong{Metadata Methods}
#'  \describe{
#'   \item{\code{docMeta(field)}}{Creates a document meta data field.}
#'   \item{\code{corpusMeta(field)}}{Creates a corpus meta data field.}
#'   \item{\code{metaVarNames()}}{Prints Corpus and Document metadata variables.}
#'  }
#'
#' \strong{Other Methods}
#'  \describe{
#'   \item{\code{logIt(level = 'Info')}}{Formats the log and calls the LogR class to log an event.}
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#'  }
#'
#' @section Parameters
#' @param dataSource A DataSource or
#' @param field Character string name for a field to be added to the document or corpus object meta data.
#' @param name Character string containing the name of the corpus object. This variable is used in the instantiation and remove methods.
#' @param path Character string containing the path to the corpus object.
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @family Corpus family of classes
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Corpus <- R6::R6Class(
  classname = "Corpus",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Meta,

  private = list(
    ..documents = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Corpus Instantiation                           #
    #-------------------------------------------------------------------------#
    initialize = function(name, path) {

      # Instantiate variables
      private$..name <- name
      private$..path <- path
      private$..className <- 'Corpus'
      private$..methodName <- 'initialize'
      private$..state <- paste0("Corpus, ", name, ", instantiated.")
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
    #                    Document Aggregation Methods                         #
    #-------------------------------------------------------------------------#
    getDocuments = function()  private$..documents,

    addDocument = function(document) {

      private$..methodName <- 'addDocument'
      name <- document$getName()
      private$..documents[[name]] <- document
      private$..state <- paste0("Added ", name, " to ", private$..name)
      self$logIt()
      invisible(self)

    },

    removeDocument = function(document) {

      private$..methodName <- 'removeDocument'
      name <- getName(document)
      private$..documents[[name]] <- NULL
      private$..state <- paste0("Removed ", name, " from ", private$..name)
      self$logIt()
      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                              IO Methods                                 #
    #-------------------------------------------------------------------------#
    read = function() {

      private$..methodName <- "read"

      content <- lapply(private$..documents, function(d) {
        d$read()
      })

      # Log it
      private$..state <- paste0("Read corpus, ", private$..name, ", into memory.")
      self$logIt()

      return(content)
    },

    write = function() {

      private$..methodName <- "write"

      lapply(private$..documents, function(d) {
        d$write()
      })

      # Log it
      private$..state <- paste0("Wrote corpus, ", private$..name, ", to disk.")
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Document MetaData Methods                         #
    #-------------------------------------------------------------------------#
    docMeta = function(key = NULL, value = NULL) {

      private$..methodName <- 'docMeta'

      # If no parameters, return meta data if available, else the metadata names
      if (is.null(key) & is.null(value)) {
        meta <- rbindlist(lapply(private$..documents, function(d) {
          m <- d$docMeta()
          if (class(m)[[1]] == 'data.frame') {
            m <- as.list(m)
          } else {
            m <- as.list(d$getName())
            names(m) <- 'Name'
          }
          m
        }))
        return(meta)
      }

      if (is.null(key)) {
        key <- names(value)
      }

      if (is.null(key)) {
        key <- paste("docMeta", seq_len(ncol(as.data.frame(value))),
                     sep = "")
      }

      if (length(value) != 1) {
        if (length(value) != length(private$..documents)) {
          private$..state <- paste0("Unable to add document metadata. ",
                                    "The value parameter must be length 1 or ",
                                    "the length equal to the number of documents ",
                                    "in the corpus.  See ?Corpus for further ",
                                    "assistance. ")
          self$logIt("Error")
          stop()
        }
      } else {
        value <- rep(value, length(private$..documents))
      }

      if (length(key) == 1) key <- rep(key, length(value))

      lapply(seq_along(value), function(m) {
        private$..documents[[m]]$docMeta(key = key[[m]], value = value[[m]])
      })

      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                         Corpus MetaData Methods                        #
    #-------------------------------------------------------------------------#
    corpusMeta = function(key = NULL, value = NULL) {

      private$..methodName <- 'corpusMeta'

      if (is.null(key) & is.null(value)) {
        if (length(private$..meta$corpus) == 0) {
          return(names(private$..meta$corpus))
        } else {
          meta <- Filter(Negate(is.null), private$..meta$corpus)
          return(as.data.frame(meta))
        }
      }

      if (is.null(key)) {
        key <- names(value)
      }

      if (is.null(key)) {
        key <- paste("corpusMeta", seq_len(ncol(as.data.frame(value))),
                     sep = "")
      }

      private$..meta$corpus[[key]] <- value

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #               MetaData Description and Summary Methods                  #
    #-------------------------------------------------------------------------#
    metaVarNames = function() {
      cat("\nCorpus metadata variable names:\n")
      print(names(private$..meta$corpus))
      cat("\nDocument metadata variable names:\n")
      print(names(private$..meta$document))
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
        name = private$..name,
        path = private$..path,
        locked = private$..locked,
        logs = private$..logs,
        state = private$..state,
        modified = private$..modified,
        created = private$..created,
        documents = private$..documents,
        docMeta = self$docMeta(),
        corpusMeta = self$corpusMeta()
      )

      return(corpus)
    }
  )
)
