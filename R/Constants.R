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
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..studioPaths = list(
      archives = "./NLPStudio/archives",
      config = "./NLPStudio/config",
      korpora = './NLPStudio/corpora',
      labs = "./NLPStudio/labs",
      logs = "./NLPStudio/logs"
    ),
    ..korpusPaths = list(
      data = "data",
      external = 'data/external',
      raw = 'data/raw',
      sets = 'data/sets',
      reports = 'reports'
    )
  ),

  public = list(

    # Constants
    getStudioPaths = function() private$..studioPaths,
    getKorpusPaths = function() private$..korpusPaths
  )
)
