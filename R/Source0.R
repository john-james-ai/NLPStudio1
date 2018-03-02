#==============================================================================#
#                             Source0                                    #
#==============================================================================#
#' Source0
#'
#' \code{Source0} Abstract class for the Source sub-classes
#'
#' @template corpusSourceStrategyClasses
#'
#' @section Source0 Methods:
#' @template corpusSourceStrategyMethods
#'
#' @template corpusSourceStrategyParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Source family of classes
#' @export
Source0 <- R6::R6Class(
  classname = "Source0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..corpus = character(),
    ..source = character(),
    ..documents = list(),
    ..name = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function(name, corpusSource) { stop("This method is not implemented for this abstract class.") },
    build = function() { stop("This method is not implemented for this abstract class.") },
    getResult = function() { stop("This method is not implemented for this abstract class.") },

    #-------------------------------------------------------------------------#
    #                             Getter Methods                              #
    #-------------------------------------------------------------------------#
    getParams = function() private$..source

  )
)
