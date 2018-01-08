#==============================================================================#
#                             PipelineBuilderData0                             #
#==============================================================================#
#' PipelineBuilderData0
#'
#' \code{PipelineBuilderData0} Abstract class for the builder for the data pipeline object.
#'
#' @template pipelineClasses
#'
#' @section PipelineBuilderData0 Methods:
#'  \describe{
#'   \item{\code{new(name, path)}}{Not implemented for this abstract class.}
#'   \item{\code{buildDataRaw(dataSource)}}{Builds the raw corpus from external data sources. The external data source is stored in the pipeline's external data directory. The raw corpus is created, stored in the pipeline's raw data directory and the corpus object (sans the text) is retained in memory.}
#'   \item{\code{buildRepaired()}}{Builds and repairs the raw data, stores it in the pipeline's repaired data directory, and returns it as a FileCollection object.}
#'   \item{\code{buildCV()}}{Builds the cross-validation sets, stores them in the pipeline's cross-validation directories, and returns it as a FileCollection object.}
#'   \item{\code{buildProcessed()}}{Builds the training set, stores it in the pipeline's cross-validation directories, and returns it as a FileCollection object.}
#'   \item{\code{getResult()}}{Obtains the collection of data / FileCollection objects, creates the PipelineData object and returns it to the calling environment.}
#'   \item{\code{accept(visitor)}}{Not implemented for this abstract class.}
#'   \item{\code{logIt(level = 'Info')}}{Not implemented for this abstract class.}
#' }
#'
#' @section Parameters:
#' @param name Character string representing the name of the Pipeline object.
#' @param path Character string representing the relative path in which the Pipeline object data will be stored.
#'
#' @docType class
#' @family Pipeline classes
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
PipelineBuilderData0 <- R6::R6Class(
  classname = "PipelineBuilderData0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..raw = list(),
    ..repaired = list(),
    ..cv = list(),
    ..processed = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                     Pipeline Initialization Method                      #
    #-------------------------------------------------------------------------#
    initialize = function(name, path) { stop("This method is not implemented for this abstract class.") },

    #-------------------------------------------------------------------------#
    #                              Build Methods                              #
    #-------------------------------------------------------------------------#
    buildDataRaw = function() { stop("This method is not implemented for this abstract class.") },


    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  { stop("This method is not implemented for this abstract class. ") }
  )
)
