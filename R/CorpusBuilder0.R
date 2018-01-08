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
  inherit = Entity,

  private = list(
    ..corpus = character(),
    ..dataSource = character(),
    ..documents = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                     Pipeline Initialization Method                      #
    #-------------------------------------------------------------------------#
    initialize = function(name, path, dataSource) { stop("This method is not implemented for this abstract class.") },

    #-------------------------------------------------------------------------#
    #                              Build Methods                              #
    #-------------------------------------------------------------------------#
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
