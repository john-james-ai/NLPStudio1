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
#' \strong{Document0data Methods}
#'  \describe{
#'   \item{\code{docMeta(field)}}{Creates a document meta data field.}
#'   \item{\code{corpusDocument0(field)}}{Creates a corpus meta data field.}
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
#' @param key Character string name for a field to be added to the Document or Corpus object metadata.
#' @param value Character string, numeric, logical, or integer value to be assigned to the key value in the Document or Corpus metadata 
#' @param name Character string containing the name of the Corpus object. This variable is used in the instantiation and remove methods.
#' @param path Character string containing the path to the Corpus object.
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
  inherit = Entity,

  private = list(
    ..documents = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Corpus Instantiation                            #
    #-------------------------------------------------------------------------#
    initialize = function(name) {

      # Instantiate variables
      private$..meta[['name']] <- name
      private$..className <- 'Corpus'
      private$..methodName <- 'initialize'
      private$..meta[['user']] <- Sys.info()["user"]
      private$..meta[["created"]] <- Sys.time()
      private$..meta[["modified"]] <- Sys.time()
      private$..meta[["accessed"]] <- Sys.time()
      private$..logs <- LogR$new()

      if (private$validateParams()$code == FALSE) stop()

      private$..meta[["id"]] <- private$createId()

      # Create log entry
      private$..state <- paste0("Corpus, ", name, ", instantiated.")
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                         Document Methods                                #
    #-------------------------------------------------------------------------#
    getDocuments = function()  { private$..documents },

    addDocument = function(document) {

      private$..methodName <- 'addDocument'
      
      # Perform validation
      v <- Validator$new()
      status <- v$addChild(self, document)
      if (status$code == FALSE) {
        private$..state <- status$msg
        self$logIt("Error")
        stop()
      }
      id <- document$getId()
      private$..documents[[id]] <- document
      private$..state <- paste0("Added Document id", id, " to ", private$..meta[["name"]])
      self$logIt()
      invisible(self)

    },

    removeDocument = function(document) {

      private$..methodName <- "removeDocument"

      if (!("Document" %in% class(document))) {
        private$..state <- paste0("Unable to remove document. Document parameter ",
                                  "is not a valid Document class object. ",
                                  "See ?", class(self)[1], " for further ",
                                  "assistance.")
        self$logIt("Error")
        stop()
      }

      id <- document$getId()
      private$..documents[[id]] <- NULL
      private$..state <- paste0("Removed Document id", id, " from ", private$..meta[["name"]])
      self$logIt()
      invisible(self)

    },
    
    purgeDocuments = function() {
      private$..methodName <- 'purgeDocuments'
      private$..documents <- list()
      private$..state <- paste0("Purged documents from ", private$..meta[["name"]])
      self$logIt()
      invisible(self)
    },
    
    purgeContent = function() {
      private$..methodName <- 'purgeContent'
      lapply(private$..documents, function(d) {
        d$text <- NULL
      })
      private$..state <- paste0("Purged content from ", private$..meta[["name"]])
      self$logIt()
      invisible(self)
    },
    
    #-------------------------------------------------------------------------#
    #                         Data and Analysis Methods                       #
    #-------------------------------------------------------------------------#
    getDNA = function(id = NULL, type = NULL) {
      
      if (!is.null(id)) {
        dna <- lapply(private$..documents, function(d) {
          d$getDNA(id = id)
        })
      } else {
        dna <- rbindlist(lapply(private$..documents, function(d) {
          d$getDNA(type = type)
        }))
      }
      return(dna)
    },

    #-------------------------------------------------------------------------#
    #                               IO Methods                                #
    #-------------------------------------------------------------------------#
    read = function(path = NULL, io = NULL) {

      private$..methodName <- "read"

      content <- lapply(private$..documents, function(d) {
        if (!is.null(path)) {
          path <- file.path(path, d$getFileName())
        }
        d$read(path = path, io = io)
      })
      private$..meta[["accessed"]] <- Sys.time()
      private$..state <- paste0("Read corpus documents")
      self$logIt()

      return(content)
    },

    write = function(path, io = NULL) {

      private$..methodName <- "write"

      lapply(private$..documents, function(d) {
        if (!is.null(d$getFileName())) {
          fileName <- d$getFileName()
        } else {
          fileName <- paste0(d$getName(), ".txt")
          d$meta(key = "fileName", value = fileName)
        }
        path <- file.path(path, fileName)

        if (is.null(io))   io <- IOText$new()

        d$write(path = path, io = io)
      })

      private$..state <- paste0("Write corpus to ", path, ".")
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                          Metadata Methods                               #
    #-------------------------------------------------------------------------#
    docMeta = function(key = NULL, value = NULL) {

      private$..methodName <- 'docMeta'

      # If no parameters, return meta data if available, else the metadata names
      if (is.null(key) & is.null(value)) {
        meta <- rbindlist(lapply(private$..documents, function(d) {
          m <- as.list(d$meta())
        }))
        return(meta)
      }

      # Format key value pairs
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

      # Apply meta data to documents
      private$..documents <- lapply(seq_along(value), function(m) {
        private$..documents[[m]]$meta(key = key[[m]], value = value[[m]])
      })

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #               Metadata Description and Summary Methods                  #
    #-------------------------------------------------------------------------#
    metaVarNames = function() {
      cat("\nMetadata variable names:\n")
      print(names(private$..meta))
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

      corpus <- list(
        name = private$..meta[["name"]],
        path = private$..path,
        logs = private$..logs,
        state = private$..state,
        modified = private$..meta[["modified"]],
        created = private$..meta[["created"]],
        documents = private$..documents,
        docMeta = self$docMeta()
      )

      return(corpus)
    }
  )
)
