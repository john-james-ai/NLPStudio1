#==============================================================================#
#                               Document0                                      #
#==============================================================================#
#' Document0
#'
#' \code{Document0} Abstract class that defines the interface for the document sub-classes
#'
#' This abstract class defines the interface for the family of document classes
#' responsible for creating, manipulating, processing, and transforming
#' documents.
#'
#' @section Document Family Participants:
#'  \itemize{
#'   \item{Document0}{This abstract class defines the interface for the other classes.}
#'   \item{DocumentText}{Class for text documents.}
#'   \item{DocumentNGrams}{Class for NGrams.}
#'   \item{DocumentPOSTags}{Class for POS tags.}
#'   }
#'
#' @section Document methods:
#'  \itemize{
#'   \item{\code{new(filePath, desc = NULL)}}{Method for instantiating a document.}
#'   \item{\code{name()}}{Method for setting or retrieving the Document object name.}
#'   \item{\code{desc()}}{Method for setting or retrieving the Document object description.}
#'   \item{\code{getName()}}{Method that returns the name of the Document object.}
#'   \item{\code{getFileName()}}{Method for obtaining the document file name.}
#'   \item{\code{getPath()}}{Method for obtaining the document path.}
#'   \item{\code{readDocument(io)}}{Method for initiating the read operation for a document.}
#'   \item{\code{writeDocument(io)}}{Method for initiating the write operation for a document.}
#'  }
#'
#' @param desc Character string containing the document description.
#' @param filePath Character string indicating the file path for a document.
#' @param io An object of one of the IO classes used for reading and writing.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document classes
#' @export
Document0 <- R6::R6Class(
  classname = "Document0",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..className = 'Document',
    ..methodName = character(),
    ..name = character(),
    ..fileName = character(),
    ..desc = character(),
    ..parent = character(),
    ..path = character(),
    ..content = character(),
    ..size = numeric(),
    ..state = character(),
    ..logs = character(),
    ..created = character(),
    ..modified = character(),
    ..accessed = character()
  ),

  #-------------------------------------------------------------------------#
  #                           Active Bindings                               #
  #-------------------------------------------------------------------------#
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
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(filePath, desc = NULL) stop("This method is not implemented for this abstract class."),
    #-------------------------------------------------------------------------#
    #                           Basic Get / Set Methods                       #
    #-------------------------------------------------------------------------#
    getName = function() private$..name,
    getFileName = function() private$..fileName,
    getPath = function() private$..path,
    setContent = function(content) private$..content <- content,

    #-------------------------------------------------------------------------#
    #                            IO Methods                                   #
    #-------------------------------------------------------------------------#
    readDocument = function(io = NULL) stop("This method is not implemented for this abstract class."),
    writeDocument = function(io = NULL) stop("This method is not implemented for this abstract class."),

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
    }
  )
)
