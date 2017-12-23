#==============================================================================#
#                                 Corpus0                                       #
#==============================================================================#
#' Corpus0
#'
#' \code{Corpus0} Class that defines a corpus or collection of documents
#'
#' Class contains a collection of text documents along with document
#' transformations such as NGrams, and POS tagged documents.
#'
#' @section Corpus0 Core Methods:
#'  \describe{
#'   \item{\code{new(name, path)}}{Creates an object of Corpus0 Class}
#'   \item{\code{getName()}}{Returns the name of the Corpus0 object.}
#'   \item{\code{getPath()}}{Returns the path of the Corpus0 object.}
#'  }
#'
#' @section IO Methods:
#'  \describe{
#'   \item{\code{read(io = NULL)}}{Reads a corpus into the Corpus0 object.}
#'   \item{\code{write(io = NULL)}}{Writes a Corpus0 object to file.}
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
#' @param field Character string name for a field to be added to the Document or Corpus0 object meta data.
#' @param name A character string containing the name of the Corpus0 object. This variable is used in the instantiation and remove methods.
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Corpus0 <- R6::R6Class(
  classname = "Corpus0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..content = list(),
    ..meta = list(
      corpus = list(),
      document = list()
    )
  ),

  active = list(
    contributor = function(value = NULL) return(private$..contributor   <- ifelse(is.null(value),  private$..contributor,   value)),
    coverage = function(value = NULL) return(private$..coverage   <- ifelse(is.null(value),  private$..coverage,   value)),
    creator = function(value = NULL) return(private$..creator   <- ifelse(is.null(value),  private$..creator,   value)),
    date = function(value = NULL) return(private$..date   <- ifelse(is.null(value),  private$..date,   value)),
    description = function(value = NULL) return(private$..description   <- ifelse(is.null(value),  private$..description,   value)),
    format = function(value = NULL) return(private$..format   <- ifelse(is.null(value),  private$..format,   value)),
    identifier = function(value = NULL) return(private$..identifier   <- ifelse(is.null(value),  private$..identifier,   value)),
    language = function(value = NULL) return(private$..language   <- ifelse(is.null(value),  private$..language,   value)),
    publisher = function(value = NULL) return(private$..publisher   <- ifelse(is.null(value),  private$..publisher,   value)),
    relation = function(value = NULL) return(private$..relation   <- ifelse(is.null(value),  private$..relation,   value)),
    rights = function(value = NULL) return(private$..rights   <- ifelse(is.null(value),  private$..rights,   value)),
    source = function(value = NULL) return(private$..source   <- ifelse(is.null(value),  private$..source,   value)),
    subject = function(value = NULL) return(private$..subject   <- ifelse(is.null(value),  private$..subject,   value)),
    title = function(value = NULL) return(private$..title   <- ifelse(is.null(value),  private$..title,   value)),
    type = function(value = NULL) return(private$..type   <- ifelse(is.null(value),  private$..type,   value)),

    type = function(value = NULL) return(private$..type   <- ifelse(is.null(value),  private$..type,   value)),
    identifier = function(value = NULL) return(private$..identifier   <- ifelse(is.null(value),  private$..identifier,   value)),
    title = function(value = NULL) return(private$..title   <- ifelse(is.null(value),  private$..title,   value)),
    alternative = function(value = NULL) return(private$..alternative   <- ifelse(is.null(value),  private$..alternative,   value)),
    abstract = function(value = NULL) return(private$..abstract   <- ifelse(is.null(value),  private$..abstract,   value)),
    extent = function(value = NULL) return(private$..extent   <- ifelse(is.null(value),  private$..extent,   value)),
    language = function(value = NULL) return(private$..language   <- ifelse(is.null(value),  private$..language,   value)),
    itemType = function(value = NULL) return(private$..itemType   <- ifelse(is.null(value),  private$..itemType,   value)),
    itemFormat = function(value = NULL) return(private$..itemFormat   <- ifelse(is.null(value),  private$..itemFormat,   value)),
    rights = function(value = NULL) return(private$..rights   <- ifelse(is.null(value),  private$..rights,   value)),
    accessRights = function(value = NULL) return(private$..accessRights   <- ifelse(is.null(value),  private$..accessRights,   value)),
    accrualMethod = function(value = NULL) return(private$..accrualMethod   <- ifelse(is.null(value),  private$..accrualMethod,   value)),
    accrualPeriodicity = function(value = NULL) return(private$..accrualPeriodicity   <- ifelse(is.null(value),  private$..accrualPeriodicity,   value)),
    accrualPolicy = function(value = NULL) return(private$..accrualPolicy   <- ifelse(is.null(value),  private$..accrualPolicy,   value)),
    provenance = function(value = NULL) return(private$..provenance   <- ifelse(is.null(value),  private$..provenance,   value)),
    audience = function(value = NULL) return(private$..audience   <- ifelse(is.null(value),  private$..audience,   value)),
    subject = function(value = NULL) return(private$..subject   <- ifelse(is.null(value),  private$..subject,   value)),
    spatial = function(value = NULL) return(private$..spatial   <- ifelse(is.null(value),  private$..spatial,   value)),
    temporal = function(value = NULL) return(private$..temporal   <- ifelse(is.null(value),  private$..temporal,   value)),
    created = function(value = NULL) return(private$..created   <- ifelse(is.null(value),  private$..created,   value)),
    dateItemsCreated = function(value = NULL) return(private$..dateItemsCreated   <- ifelse(is.null(value),  private$..dateItemsCreated,   value)),
    creator = function(value = NULL) return(private$..creator   <- ifelse(is.null(value),  private$..creator,   value)),
    OWN = function(value = NULL) return(private$..OWN   <- ifelse(is.null(value),  private$..OWN,   value)),
    isLocatedAt = function(value = NULL) return(private$..isLocatedAt   <- ifelse(is.null(value),  private$..isLocatedAt,   value)),
    isAccessedVia = function(value = NULL) return(private$..isAccessedVia   <- ifelse(is.null(value),  private$..isAccessedVia,   value)),
    hasPart = function(value = NULL) return(private$..hasPart   <- ifelse(is.null(value),  private$..hasPart,   value)),
    isPartOf = function(value = NULL) return(private$..isPartOf   <- ifelse(is.null(value),  private$..isPartOf,   value)),
    catalogueOrIndex = function(value = NULL) return(private$..catalogueOrIndex   <- ifelse(is.null(value),  private$..catalogueOrIndex,   value)),
    associatedCollection = function(value = NULL) return(private$..associatedCollection   <- ifelse(is.null(value),  private$..associatedCollection,   value)),
    isReferencedBy = function(value = NULL) return(private$..isReferencedBy   <- ifelse(is.null(value),  private$..isReferencedBy,   value))
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Corpus0 Instantiation                            #
    #-------------------------------------------------------------------------#
    initialize = function(name, path = NULL) {

      # Instantiate variables
      private$..admin$className <- 'Corpus0'
      private$..admin$methodName <- 'initialize'
      private$..admin$name <- name
      private$..admin$path <- path
      private$..admin$state <- paste0("Corpus0, ", name, ", instantiated.")
      private$..admin$modified <- Sys.time()
      private$..admin$created <- Sys.time()
      private$..admin$accessed <- Sys.time()
      private$..admin$logs <- LogR$new()

      # Validate Corpus0
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

    getCorpus0 = function() invisible(self),

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
    #                       Corpus0 Sourcing Methods                           #
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
