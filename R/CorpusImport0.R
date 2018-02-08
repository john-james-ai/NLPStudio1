#==============================================================================#
#                             CorpusImport0                                    #
#==============================================================================#
#' CorpusImport0
#'
#' \code{CorpusImport0} Abstract class for the CorpusImport sub-classes
#'
#' @template corpusImportStrategyClasses
#'
#' @section CorpusImport0 Methods:
#' @template corpusImportStrategyMethods
#'
#' @template corpusImportStrategyParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusImport family of classes
#' @export
CorpusImport0 <- R6::R6Class(
  classname = "CorpusImport0",
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
    getDataSource = function() private$..dataSource

  )
)
