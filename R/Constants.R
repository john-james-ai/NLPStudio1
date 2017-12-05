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
      config = "./NLPStudio/config",
      labs = "./NLPStudio/labs",
      korpora = './NLPStudio/corpora',
      logs = "./NLPStudio/logs",
      archives = "./NLPStudio/archives"
    ),
    ..labsPaths = list(
      korpora = 'corpora',
      archives = 'archives',
      logs = 'logs'
    ),
    ..korpusPaths = list(
      data = "data",
      external = 'data/external',
      raw = 'data/raw',
      sets = 'data/sets',
      reports = 'reports',
      logs = 'logs'
    )
  ),

  public = list(

    # Constants
    getStudioPaths = function() private$..studioPaths,
    getLabsPaths = function() private$..labsPaths,
    getKorpusPaths = function() private$..korpusPaths
  )
)
