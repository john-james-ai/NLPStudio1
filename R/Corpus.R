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
    ..io = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Corpus Instantiation                            #
    #-------------------------------------------------------------------------#
    initialize = function(name, io, desc = NULL) {

      # Instantiate variables
      private$..className <- 'Corpus'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "corpus"), desc)
      private$..parent <- NLPStudios$new()$getInstance()
      private$..path <- file.path(NLPStudios$new()$getInstance()$getPath(), 'corpora', private$..name)
      private$..ioIn <- ioIn
      private$..ioOut <- ioOut
      private$..state <- "Corpus instantiated."
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..logs <- LogR$new()

      # Validate Corpus
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Create directory
      dir.create(private$..path, showWarnings = FALSE,  recursive = TRUE)

      # Create log entry
      self$logIt()

      # Assign its name in the global environment
      assign(name, self, envir = .GlobalEnv)

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

      # Get collection information
      documentName <- document$getName()

      # Add document to list of documents
      private$..documents[[documentName]] <- document

      # Move document to Corpus
      document$move(self)

      # Update modified time
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

      # Move document back to main document directory
      document$move(NLPStudios$new()$getInstance())

      # Update modified time
      private$..modified <- Sys.time()

      # Save state and log Event
      private$..state <-
        paste("Document", documentName, "removed from ", private$..name, "at", Sys.time())
      self$logIt()

      invisible(self)
    },

    move = function(parent) {

      private$..methodName <- 'move'

      v <- Validator$new()
      status <- v$setParent(self, parent)

      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      } else {
        private$..parent <- parent

        private$..path <- file.path(parent$getPath(),
                                    private$..name)

        # Move documents
        private$..documents <- lapply(private$..documents, function(d) {
          d$move(self)
        })

        # Log it
        private$..modified <- Sys.time()
        private$..state <- paste(private$..className, private$..name, 'moved to ',
                                 parent$getClassName(), parent$getName())
        self$logIt()

        # Assign its name in the global environment
        assign(private$..name, self, envir = .GlobalEnv)

        invisible(self)
      }
    },

    #-------------------------------------------------------------------------#
    #                        Source Command Methods                           #
    #-------------------------------------------------------------------------#
    webSource = function(corpusSource, files, compressed = TRUE, format = 'zip') {

      private$..methodName <- 'webSource'

      # Validation
      v <- Validator$new()
      status <- v$webSource(self, corpusSource)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = "Error")
        stop()
      }

      # Format download and unzip paths, and file name.
      downloadPath <- file.path(private$..path, private$..extDir)
      unZipPath <- file.path(private$..path, private$..rawDir)
      fileName <- installr::file.name.from.url(corpusSource)
      dir.create(downloadPath,  showWarnings = FALSE, recursive = TRUE)

      if (length(list.files(file.path(private$..path, private$..extDir))) < 1) {

        # Download data
        f <- FileManager$new()
        status <- f$download(corpusSource, downloadPath)
        if (status[['code']] == FALSE) {
          private$..state <- status[['msg']]
          self$logIt(level = 'Error')
          stop()
        }
      }

      # Unzip data
      f <- FileManager$new()
      if (compressed == TRUE) {
        if (format == 'zip') {
          oldw <- getOption("warn")
          options(warn = -1)
          status[['data']] <- f$unZipFile(zipFilePath = file.path(downloadPath,
                                                                  fileName),
                                          exDir = file.path(unZipPath),
                                          files = files, list = FALSE,
                                          overwrite = FALSE)
          options(warn = oldw)

          if (status[['code']] == FALSE) {
            private$..state <- status[['msg']]
            self$logIt(level = 'Error')
            stop()
          }
        } else {
          private$..state <- paste("Only 'zip' compressed formats are supported. ",
                                   "See ?", private$..className, " for further ",
                                   "assistance. ")
          self$logIt(level = 'Error')
          stop()
        }
      } else {
        private$..state <- paste("Only compressed web sources are supported. ",
                                 "See ?", private$..className, " for further ",
                                 "assistance. ")
        self$logIt(level = 'Error')
        stop()
      }

      # Create documents
      files <- list.files(path = unZipPath, full.names = TRUE)
      documents <- lapply(files, function(f) {
        doc <- Document$new(f, ioOut)
        doc$read()
      })

      # Add documents
      lapply(documents, function(d) {
        self$addDocument(d)
        d$move(self)
      })

      # LogIt
      private$..state <- paste("Sourced Corpus object", private$..name, "from the web.")
      private$..modified <- Sys.time()
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },

    inSource = function(corpusSource) {

      private$..methodName <- 'inSource'

      # Validation
      v <- Validator$new()
      status <- v$inSource(self, corpusSource)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = "Error")
        stop()
      }

      # Create documents
      path <- dirname(private$..path)
      documents <- corpusSource$getDocuments()
      documents <- lapply(documents, function(d) {
        fileName <- d$getFileName()
        filePath <- file.path(path, fileName)
        doc <- Document$new(filePath, ioOut)
        doc$cloneContent(d)
      })

      # Add documents
      lapply(documents, function(d) {
        self$addDocument(d)
        d$move(self)
      })

      # LogIt
      private$..state <- paste0("Sourced Corpus object ", private$..name,
                               " from ", corpusSource$getName(), ". ")
      private$..modified <- Sys.time()
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
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
