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
  inherit = Entity,

  private = list(
    ..locked = FALSE,
    ..corpus = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Corpus Instantiation                            #
    #-------------------------------------------------------------------------#
    initialize = function(name, path = NULL) {

      # Instantiate variables
      private$..admin$className <- 'Corpus'
      private$..admin$methodName <- 'initialize'
      private$..admin$name <- name
      private$..admin$path <- path
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

      # Load corpus
      if (!is.null(private$..admin$path)) {
        private$..corpus == quanteda::corpus(private$..admin$path)
      }


      # Create log entry
      self$logIt()

      invisible(self)
    },

    getCorpus = function() invisible(self),

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
      private$..admin$state <- paste0("Read corpus, ", private$..admin$name, ", into memory.")
      self$logIt()

      invisible(self)
    },

    write = function() {
      private$..documents <- lapply(private$..documents, function(d) {
        d$write()
      })
    },

    #-------------------------------------------------------------------------#
    #                       Corpus Sourcing Methods                           #
    #-------------------------------------------------------------------------#
    download = function(url, name) {

      private$..admin$methodName <- 'download'

      # Create file collection object and download data
      fc <- FileCollection$new(name = name, path = file.path(private$..admin$path, name))
      fc <- fc$download(url = url)

      # Add file collection to data
      name <- fc$getName()
      private$..corpora[['name']] <- fc

      # Log it
      fileName <- basename(url)
      private$..admin$state <- paste0("Successfully downloaded and added", fileName, " to the data set. ")
      self$logIt()

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
        className = private$..admin$className,
        methodName = private$..admin$methodName,
        name = private$..admin$name,
        path = private$..admin$path,
        content = private$..content,
        locked = private$..admin$locked,
        logs = private$..admin$logs,
        state = private$..admin$state,
        modified = private$..admin$modified,
        created = private$..admin$created
      )

      return(corpus)
    }
  )
)
