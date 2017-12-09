## ---- SourceData0
#==============================================================================#
#                               SourceData0                                    #
#==============================================================================#
#' SourceData0
#'
#'
#' \code{SourceData0} Abstract class for the SourceData family of classes
#'
#' Abstract class for the SourceData classes
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{initialize()}}{Not implemented for this abstract class.}
#'  \item{\code{logIt()}}{Not implemented for this abstract class.}
#' }
#'
#' @return Character string indicating the name of the Validator class.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family SourceData Classes
#' @export
SourceData0 <- R6::R6Class(
  "SourceData0",
  private = list(
    ..className = 'SourceData0'
  ),
  public = list(
    initialize = function() stop("The initialization method is not implemented for this abstract class.")
  )
)

