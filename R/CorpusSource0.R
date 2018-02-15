#==============================================================================#
#                             CorpusSource0                                    #
#==============================================================================#
#' CorpusSource0
#'
#' \code{CorpusSource0} Abstract class for the CorpusSource sub-classes
#'
#' @template corpusSourceStrategyClasses
#'
#' @section CorpusSource0 Methods:
#' @template corpusSourceStrategyMethods
#'
#' @template corpusSourceStrategyParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusSource family of classes
#' @export
CorpusSource0 <- R6::R6Class(
  classname = "CorpusSource0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..corpus = character(),
    ..dataSource = character(),
    ..documents = list(),
    ..name = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function(name, dataSource) { stop("This method is not implemented for this abstract class.") },
    build = function() { stop("This method is not implemented for this abstract class.") },
    getResult = function() { stop("This method is not implemented for this abstract class.") },

    #-------------------------------------------------------------------------#
    #                             Getter Methods                              #
    #-------------------------------------------------------------------------#
    getParams = function() private$..dataSource

  )
)
