#==============================================================================#
#                             Builder0                                   #
#==============================================================================#
#' Builder0
#'
#' \code{Builder0} Abstract class for the Corpus builder classes
#'
#' The Document family of classes is an implementation of the builder
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows the
#' construction process to be defined at runtime.
#'
#' @section Corpus Builder Participants:
#'  \describe{
#'   \item{Builder0}{This abstract builder interface. Defines the interface for concrete corpus builder sub-classes. }
#'   \item{Builder}{Concrete builder sub-class that produces the Corpus object. }
#'   \item{CorpusSetBuilder}{Concrete builder sub-class that produces the Corpus training, validation and test set objects. }
#'   \item{CorpusDirector}{Class that builds the corpus through the concrete builder interfaces.}
#'   \item{Corpus}{The corpus or corpus set product.}
#'   }
#'
#' @section Corpus Builder0 Methods:
#'  \describe{
#'   \item{\code{new()}}{Instantiates the builder object}
#'   \item{\code{getData()}}{Obtains the corpus data from an external source.}
#'   \item{\code{buildDocuments()}}{Builds document objects.}
#'   \item{\code{repairDocuments()}}{Rapairs corpus document objects}
#'   \item{\code{splitDocuments()}}{Creates random sample of 10 indices, each representing 10 pct of the lines of the document.}
#'  }
#'
#'
#' @param name Character string indicating the name of the Corpus object
#' @param desc Character string containing the description for the corpus object
#' @param parent Object of the Corpus or Lab classes
#' @param path Character string indicating the path to the Corpus object
#' @param dirs Directory structure for a Corpus object
#' @param state Character string indicating last state of object
#' @param url Character string indicating URL for downloading data
#' @param files Character vector containing names of files to be extracted from zipped archive.
#' @param repairs list of gsub key and value pairs
#' @param sets List of Document class objects, including text, nGrams and pos tags.
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus build family
#' @export
Builder0 <- R6::R6Class(
  classname = "Builder0",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..className = 'Builder0',
    ..methodName = character(),
    ..name = character(),
    ..desc = character(),
    ..parent = character(),
    ..path = character(),
    ..state = character(),
    ..logs = character(),
    ..modified = character(),
    ..created = character()
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
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                            Build Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function() stop("This method is not implemented in this abstract class."),

    #-------------------------------------------------------------------------#
    #                            Getter Methods                               #
    #-------------------------------------------------------------------------#
    getName = function() private$..name,
    getPath = function() private$..path,

    #-------------------------------------------------------------------------#
    #                           Deliver Product                               #
    #-------------------------------------------------------------------------#
    deliver = function() stop("This method is not implemented in this abstract class."),

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor) stop("This method is not implemented in this abstract class."),

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
