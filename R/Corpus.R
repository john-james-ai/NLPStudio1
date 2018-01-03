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
#'   \item{\code{new(name, path)}}{Creates an object of Corpus Class}
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
#' @param field Character string name for a field to be added to the Document or Corpus object meta data.
#' @param name A character string containing the name of the Corpus object. This variable is used in the instantiation and remove methods.
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Corpus <- R6::R6Class(
  classname = "Corpus",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Meta,

  private = list(
    ..source = character(),
    ..documents = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Corpus Instantiation                            #
    #-------------------------------------------------------------------------#
    initialize = function(name, x) {

      # Instantiate variables
      private$..name <- name
      private$..source <- x
      private$..admin$className <- 'Corpus'
      private$..admin$methodName <- 'initialize'
      private$..admin$state <- paste0("Corpus, ", name, ", instantiated.")
      private$..admin$modified <- Sys.time()
      private$..admin$created <- Sys.time()
      private$..admin$accessed <- Sys.time()
      private$..admin$logs <- LogR$new()

      # Validate Corpus
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..admin$state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Import data
      documents <- switch(class(x)[[1]],
                          FileCollection = self$importFC()
      )
      lapply(documents, function(d) {
        self$addDocument(d)
      })

      # Create log entry
      self$logIt()

      invisible(self)
    },

    getName = function() private$..name,

    #-------------------------------------------------------------------------#
    #                           Import Methods                                #
    #-------------------------------------------------------------------------#
    importFC = function() {
      import <- CorpusImportFC$new(private$..source)
      return(import$execute())
    },

    #-------------------------------------------------------------------------#
    #                    Document Aggregation Methods                         #
    #-------------------------------------------------------------------------#
    getDocuments = function()  private$..documents,

    addDocument = function(document) {

      private$..admin$methodName <- 'addDocument'
      name <- document$getName()
      private$..documents[[name]] <- document
      private$..admin$state <- paste0("Add ", name, " to ", private$..name)
      self$logIt()
      invisible(self)

    },

    removeDocument = function(document) {

      private$..admin$methodName <- 'removeDocument'
      name <- getName(document)
      private$..documents[[name]] <- NULL
      private$..admin$state <- paste0("Removed ", name, " from ", private$..name)
      self$logIt()
      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                              IO Methods                                 #
    #-------------------------------------------------------------------------#
    read = function() {

      files <- list.files(private$..admin$path, full.names = TRUE)
      content <- lapply(files, function(f) {
        io <- IOFactory$new()$getIOStrategy(f)
        text <- io$read(f)
        text <- paste(text, collapse = " ")
        names(text) <- tools::file_path_sans_ext(basename(f))
        text
      })

      # Create corpus object
      private$..corpus <- quanteda::corpus(unlist(content))

      # Log it
      private$..admin$state <- paste0("Read corpus, ", private$..name, ", into memory.")
      self$logIt()

      invisible(self)
    },

    write = function() {
      private$..documents <- lapply(private$..documents, function(d) {
        d$write()
      })
    },

    #-------------------------------------------------------------------------#
    #                       Document MetaData Methods                         #
    #-------------------------------------------------------------------------#
    docMeta = function(key = NULL, value = NULL) {

      private$..admin$methodName <- 'docMeta'

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
    #                         Corpus MetaData Methods                         #
    #-------------------------------------------------------------------------#
    corpusMeta = function(key = NULL, value = NULL) {

      private$..admin$methodName <- 'corpusMeta'

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
        path = private$..admin$path,
        content = private$..content,
        locked = private$..admin$locked,
        logs = private$..admin$logs,
        state = private$..admin$state,
        modified = private$..admin$modified,
        created = private$..admin$created,
        documents = private$..documents,
        docMeta = self$docMeta(),
        corpusMeta = self$corpusMeta()
      )

      return(corpus)
    }
  )
)
