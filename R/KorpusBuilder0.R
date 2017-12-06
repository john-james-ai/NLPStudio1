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
#' @section Korpus Builder Participants:
#'  \describe{
#'   \item{KorpusBuilder0}{This abstract builder interface. Defines the interface for concrete corpus builder sub-classes. }
#'   \item{KorpusBuilder}{Concrete builder sub-class that produces the Korpus object. }
#'   \item{KorpusSetBuilder}{Concrete builder sub-class that produces the Korpus training, validation and test set objects. }
#'   \item{KorpusDirector}{Class that builds the corpus through the concrete builder interfaces.}
#'   \item{Korpus}{The corpus or corpus set product.}
#'   }
#'
#' @section Korpus Builder0 Methods:
#'  \describe{
#'   \item{\code{new()}}{Instantiates the builder object}
#'   \item{\code{getData()}}{Obtains the corpus data from an external source.}
#'   \item{\code{buildDocuments()}}{Builds document objects.}
#'   \item{\code{repairDocuments()}}{Rapairs corpus document objects}
#'   \item{\code{splitDocuments()}}{Creates random sample of 10 indices, each representing 10 pct of the lines of the document.}
#'  }
#'
#'
#' @param name Character string indicating the name of the Korpus object
#' @param desc Character string containing the description for the corpus object
#' @param parent Object of the Korpus or Lab classes
#' @param path Character string indicating the path to the Korpus object
#' @param dirs Directory structure for a Korpus object
#' @param documents
#' @param sets List of Document class objects, including text, nGrams and pos tags.
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
    ..className = 'KorpusBuilder0',
    ..methodName = character(),
    ..logs = character(),
    ..korpus = list(
      metaData = list(
        name = character(),
        desc = character(),
        parent = character(),
        path = character(),
        dirs = character(),
        logs = character(),
        state = character(),
        modified = character(),
        created = character()
      ),
      parameters = list(
        url = character(),
        files = character(),
        repairs = list(),
        splits = numeric(),
        samples = list(),
        normalize = list(),
        corrections = list(),
        profanity = list()
        ),
      documents = list(
        extDocs = list(),
        rawDocs = list(),
        sets = list()
      ),
    ..modified = character(),
    ..created = character()
    )
  ),

  active = list(
    #--------------------------------#
    #         Meta Data              #
    #--------------------------------#

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
    #--------------------------------#
    #          Parameters            #
    #--------------------------------#
    url = function(value) {
      if (missing(value)) {
        private$..url
      } else {
        private$..url <- value
      }
    },

    files = function(value) {
      if (missing(value)) {
        private$..files
      } else {
        private$..files <- value
      }
    },

    repairs = function(value) {
      if (missing(value)) {
        private$..repairs
      } else {
        private$..repairs <- value
      }
    },

    splits = function(value) {
      if (missing(value)) {
        private$..splits
      } else {
        private$..splits <- value
      }
    },

    samples = function(value) {
      if (missing(value)) {
        private$..samples
      } else {
        private$..samples <- value
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
