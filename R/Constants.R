#==============================================================================#
#                                 Constants                                    #
#==============================================================================#
#' Constants
#'
#' \code{Constants} Class containing constants used throughout the package such
#' as directories structures.
#'
#' @section Constants Methods:
#' \describe{
#'  \item{\code{getPaths()}}{Returns the NLPStudio directory and file paths as a list.}
#' }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Constants <- R6::R6Class(
  classname = "Constants",
  lock_objects = TRUE,
  lock_class = TRUE,

  private = list(
    ..studioPaths = list(
      config = "config",
      labs = "labs",
      logs = "logs",
      archives = archives
    ),
    ..labPaths = list(
      data = "data",
      external = 'data/external',
      raw = 'data/raw',
      munge = 'data/munge',
      processed = 'data/processed',
      reports = 'reports',
      logs = 'logs'
    )
  ),

  public = list(

    # Constants
    getStudioPaths = function() private$..studioPaths,
    getLabPaths = function() private$..labPaths
  )
)
