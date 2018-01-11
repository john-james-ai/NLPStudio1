#==============================================================================#
#                             CorpusBuilder0                                   #
#==============================================================================#
#' CorpusBuilder0
#'
#' \code{CorpusBuilder0} Abstract class for the Corpusbuilder sub-classes
#'
#' @template corpusBuilderClasses
#'
#' @section CorpusBuilder0 Methods:
#' @template corpusBuilderMethods
#'
#' @template corpusBuilderParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusBuilder family of classes
#' @export
CorpusBuilder0 <- R6::R6Class(
  classname = "CorpusBuilder0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Corpus,

  private = list(
    ..corpus = character(),
    ..dataSource = character(),
    ..documents = character(),
    ..name = character(),
    ..path = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function(name, path, dataSource) { stop("This method is not implemented for this abstract class.") },
    getName = function() private$..name,
    getPath = function() private$..path,
    getSource = function() private$..dataSource,

    #-------------------------------------------------------------------------#
    #                              Build Methods                              #
    #-------------------------------------------------------------------------#
    buildDocuments = function() { stop("This method is not implemented for this abstract class.") },
    buildCorpus = function() { stop("This method is not implemented for this abstract class.") },
    getResult = function() { stop("This method is not implemented for this abstract class.") }

  )
)
