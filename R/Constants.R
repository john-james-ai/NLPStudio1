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
    ..paths = list(
      studio = "./NLPStudio",
      labs = "./NLPStudio/labs",
      log = "./NLPStudio/Log",
      test = './NLPStudio/Test'
    )
  ),

  public = list(

    # Constants
    getPaths = function() private$..constants$paths
  )
)
