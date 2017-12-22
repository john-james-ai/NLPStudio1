## ---- DataSource0
#==============================================================================#
#                               DataSource0                                    #
#==============================================================================#
#' DataSource0
#'
#'
#' \code{DataSource0} Abstract class for the DataSource classes.
#'
#' Abstract class for the DataSource classes.
#'
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(name, path, params)}}{Instantiates an object of one of the DataSource classes.}
#'  \item{\code{execute()}}{Sources the data, creates the FileCollection object, and returns it to the calling environment.}
#' }
#'
#' @return collection FileCollection object
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Data source classes
#' @export
DataSource0 <- R6::R6Class(
  "DataSource0",
  inherit = Entity,

  private = list(
    ..path = character(),
    ..params = list()
  ),

  public = list(
    initialize = function(name, path, params) stop("The method is not implemented for this abstract class."),
    execute = function() stop("The method is not implemented for this abstract class."),
    getParams = function() private$..params
  )
)

