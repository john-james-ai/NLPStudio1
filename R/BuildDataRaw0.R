#==============================================================================#
#                               BuildDataRaw0                                      #
#==============================================================================#
#' BuildDataRaw0
#'
#' \code{BuildDataRaw0} Abstract class for the BuildDataRaw family of classes.
#'
#' @template buildDataRawClasses
#' @template buildDataRawMethods
#' @template buildDataRawParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Build raw data classes
#' @export
BuildDataRaw0 <- R6::R6Class(
  "BuildDataRaw0",
  inherit = Entity,

  private = list(
    ..pipeline = character(),
    ..dataSource = character()
  ),

  public = list(
    initialize = function(pipeline, dataSource) stop("The method is not implemented for this abstract class."),
    execute = function() stop("The method is not implemented for this abstract class.")
  )
)

