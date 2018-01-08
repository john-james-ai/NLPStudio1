#==============================================================================#
#                               DataSource0                                    #
#==============================================================================#
#' DataSource0
#'
#' \code{DataSource0} Abstract class for the DataSource family of classes.
#'
#' @template dataSourceClasses
#' @template  dataSourceMethods
#' @template  dataSourceParams
#'
#' @return dataSource: An object of one of the DataSource classes.
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Source data classes
#' @export
DataSource0 <- R6::R6Class(
  "DataSource0",
  inherit = Entity,

  private = list(
    ..dataSource = NULL
  ),

  public = list(
    initialize = function(dataSource) stop("The method is not implemented for this abstract class."),
    getDataSource = function() private$..dataSource
  )
)

