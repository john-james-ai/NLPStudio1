#==============================================================================#
#                             KorpusBuilder0                                   #
#==============================================================================#
#' KorpusBuilder0
#'
#' \code{KorpusBuilder0} Abstract class for the Korpus builder classes
#'
#' The Document family of classes is an implementation of the builder
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows the
#' construction process to be defined at runtime.
#'
#' @section Korpus Builder Participants
#'  \describe{
#'   \item{KorpusBuilder0}{This abstract builder interface. Defines the interface for concrete corpus builder sub-classes. }
#'   \item{KorpusDirector}}{Class that builds the corpus through the concrete builder interface.}
#'   \item{Korpus}}{The corpus or corpus set product.}
#'
#' @section Korpus Builder0 Methods:
#'  \describe{
#'   \item{\code{new()}}{Instantiates the builder object}
#'   \item{\code{getData()}}{Obtains the corpus data from an external source.}
#'   \item{\code{buildDocuments()}}{Builds document objects.}
#'   \item{\code{repairDocuments()}}{Rapairs corpus document objects}
#'   \item{\code{splitDocuments()}}{Creates random sample of 10 indices, each representing 10% of the lines of the document..}
#'   \item{\code{sampleDocuments()}}{Creates sammples of the document according to size increments of 10%.}
#'   \item{\code{normalizeDocuments()}}{Normalizes text in the document objects.}
#'   \item{\code{correctDocuments()}}{Corrects common misspellings, contractions, and abbreviations.}
#'   \item{\code{cleanLanguage()}}{Removes sentences containing profane language.}
#'   \item{\code{nGramDocuments()}}{Creates nGram representations of document objects.}
#'   \item{\code{posTagDocuments()}}{Creates POS tag representations of document objects.}
#'   \item{\code{deliverKorpus()}}{Returns Korpus object.}
#'  }
#'
#' @param name Character string indicating the name of the Korpus object
#' @param desc Character string containing the description for the corpus object
#' @param path Character string indicating the path to the Korpus object
#' @param extDir Character string indicating the directory into which the external files will be stored
#' @param getData Boolean, indicator to obtain the data from an external source. Default is TRUE
#' @param documents Boolean, indicates whether document objects are to be created. Default is TRUE
#' @param repair Boolean, indicates whether the documents should undergo repair. Default is TRUE
#' @param split Boolean, indicates whether documents splits should be created. Default is TRUE
#' @param sample Boolean, indicates whether document samples should be taken. Default is TRUE
#' @param normalize Boolean, indicates whether text normalization should take place.Default is TRUE
#' @param correct Boolean, to perform corrections. Default is TRUE
#' @param profanity Boolean, indicator to remove profanity. Default is TRUE
#' @param nGrams Numeric indicator of the number of nGrams to build
#' @param posTags Boolean, indicates whether to create POS tags.  Default is TRUE
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus build family
#' @export
KorpusBuilder0 <- R6::R6Class(
  classname = "KorpusBuilder0",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..className = 'KorpusBuilder',
    ..methodName = character(),
    ..name = character(),
    ..desc = character(),
    ..parent = character(),
    ..path = character(),
    ..url = character(),
    ..files = character(),
    ..dirs = list(),
    ..extDocs = list(),
    ..rawDocs = list(),
    ..repairs = list(),
    ..splits = numeric(),
    ..normalize = list(),
    ..corrections = list(),
    ..profanity = list(),
    ..nGrams = numeric(),
    ..sets = list(),
    ..logs = character(),
    ..reports = list(),
    ..state = character(),
    ..modified = "None",
    ..created = "None"
  ),

  active = list(

    name = function(value) {
      if (missing(value)) {
        private$..name
      } else {
        private$..name <- value
      }
    },

    desc = function(value) {
      if (missing(value)) {
        private$..desc
      } else {
        private$..desc <- value
      }
    },

    path = function(value) {
      if (missing(value)) {
        private$..path
      } else {
        private$..path <- value
      }
    },

    dirs = function(value) {
      if (missing(value)) {
        private$..dirs
      } else {
        private$..dirs <- value
      }
    },

    url = function(value) {
      if (missing(value)) {
        private$..$url
      } else {
        private$..$url <- value
      }
    },


    files = function(value) {
      if (missing(value)) {
        private$..$files
      } else {
        private$..$files <- value
      }
    },

    extDocs = function(value) {
      if (missing(value)) {
        private$..extDocs
      } else {
        private$..extDocs <- value
      }
    },

    repairs = function(value) {
      if (missing(value)) {
        self$repairs
      } else {
        self$repairs <- value
      }
    },

    rawDocs = function(value) {
      if (missing(value)) {
        private$..rawDocs
      } else {
        private$..rawDocs <- value
      }
    },

    splits = function(value) {
      if (missing(value)) {
        private$..splits
      } else {
        private$..splits <- value
      }
    },

    normalize = function(value) {
      if (missing(value)) {
        private$..normalize
      } else {
        private$..normalize <- value
      }
    },

    corrections = function(value) {
      if (missing(value)) {
        private$..corrections
      } else {
        private$..corrections <- value
      }
    },

    profanity = function(value) {
      if (missing(value)) {
        private$..profanity
      } else {
        private$..profanity <- value
      }
    },

    nGrams = function(value) {
      if (missing(value)) {
        private$..nGrams
      } else {
        private$..nGrams <- value
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                            Build Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function() stop("This method is not implemented in this abstract class."),
    getData = function() stop("This method is not implemented in this abstract class."),
    buildDocuments = function() stop("This method is not implemented in this abstract class."),
    repairDocuments = function() stop("This method is not implemented in this abstract class."),
    splitDocuments = function() stop("This method is not implemented in this abstract class."),
    sampleDocuments = function() stop("This method is not implemented in this abstract class."),
    normalizeDocuments = function() stop("This method is not implemented in this abstract class."),
    correctDocuments = function() stop("This method is not implemented in this abstract class."),
    removeProfanity = function() stop("This method is not implemented in this abstract class."),
    createNGrams = function() stop("This method is not implemented in this abstract class."),
    createPOSTags = function() stop("This method is not implemented in this abstract class."),

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$korpus(self)
    },

    #-------------------------------------------------------------------------#
    #                            Log Method                                   #
    #-------------------------------------------------------------------------#
    logIt = function(level = 'Info', fieldName = NA) {

      private$..logs$entry$owner <- private$..name
      private$..logs$entry$className <- private$..className
      private$..logs$entry$methodName <- private$..methodName
      private$..logs$entry$level <- level
      private$..logs$entry$msg <- private$..state
      private$..logs$entry$fieldName <- fieldName
      private$..logs$created <- Sys.time()
      private$..logs$writeLog()
    },

    #-------------------------------------------------------------------------#
    #                           Test Methods                                  #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      #TODO: Remove after testing

      builder = list(
        className = private$..className,
        methodName = private$..methodName,
        name = private$..name,
        documents = private$..documents,
        builder = private$..builder,
        state = private$..state,
        modified = private$..modified,
        created = private$..created
      )

      return(builder)
    }
  )
)
