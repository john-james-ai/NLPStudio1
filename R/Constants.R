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
#'  \item{\code{getPaths()}}{Returns the NLPStudios directory and file paths as a list.}
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
    ..studiosPaths = list(
      archives = "./NLPStudios/archives",
      config = "./NLPStudios/config",
      corpora = './NLPStudios/corpora',
      studios = "./NLPStudios/studios",
      logs = "./NLPStudios/logs"
    ),
    ..corpusPaths = list(
      data = "data",
      external = 'data/external',
      raw = 'data/raw',
      pre = 'data/preprocessed',
      sets = 'data/sets',
      reports = 'reports'
    )
  ),

  public = list(

    # Constants
    getStudiosPaths = function() private$..studiosPaths,
    getCorpusPaths = function() private$..corpusPaths
  )
)
