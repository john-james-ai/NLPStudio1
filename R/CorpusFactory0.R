#==============================================================================#
#                             CorpusFactory0                                   #
#==============================================================================#
#' CorpusFactory0
#'
#' \code{CorpusFactory0} Abstract class for the CorpusFactory family of classes.
#'
#' This abstract class defines the interface for the classes responsible
#' for the data / corpus preparation stages of the pipeline, including
#' corpus repair, reshaping and splitting into cross-validation sets.
#'
#' @template corpusFactoryClasses
#'
#' @section CorpusFactory0 Methods:
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusImport family of classes
#' @export
CorpusFactory0 <- R6::R6Class(
  classname = "CorpusFactory0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..inCorpus = character(),
    ..name = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function(inCorpus, name, ...) { stop("This method is not implemented for this abstract class.") },
    build = function() { stop("This method is not implemented for this abstract class.") }

  )
)
