#==============================================================================#
#                             CorpusBuilderRaw                                 #
#==============================================================================#
#' CorpusBuilderRaw
#'
#' \code{CorpusBuilderRaw} Concrete builder class for raw Corpus objects.
#'
#' @template corpusBuilderClasses
#'
#' @section CorpusBuilderRaw Methods:
#' @template corpusBuilderMethods
#'
#' @template corpusBuilderParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusBuilder family of classes
#' @export
CorpusBuilderRaw <- R6::R6Class(
  classname = "CorpusBuilderRaw",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CorpusBuilder0,

  public = list(

    #-------------------------------------------------------------------------#
    #                     Pipeline Initialization Method                      #
    #-------------------------------------------------------------------------#
    initialize = function(name, path, dataSource) {

      private$..corpus <- Corpus$new(name = name, path = path)
      private$..dataSource <- dataSource

      private$..admin$className <- 'CorpusBuilderRaw'
      private$..admin$methodName <- 'initialize'
      private$..admin$state <- paste0("CorpusBuilderRaw object instantiated.")
      private$..admin$created <- Sys.time()
      private$..admin$modified <- Sys.time()
      private$..admin$accessed <- Sys.time()
      private$..admin$logs <- LogR$new()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                              Build Methods                              #
    #-------------------------------------------------------------------------#
    buildData = function() { stop("This method is not implemented for this abstract class.") },
    buildDocuments = function() { stop("This method is not implemented for this abstract class.") },
    buildCorpus = function() { stop("This method is not implemented for this abstract class.") },
    getResult = function() { stop("This method is not implemented for this abstract class.") },

    #-------------------------------------------------------------------------#
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  { stop("This method is not implemented for this abstract class. ") },
    logIt = function(level = 'Info') { stop("This method is not implemented for this abstract class. ") }
  )
)
