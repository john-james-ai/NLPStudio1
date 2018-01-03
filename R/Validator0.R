## ---- Validator0
#==============================================================================#
#                               Validator0                                     #
#==============================================================================#
#' Validator0
#'
#'
#' \code{Validator0} Abstract class for the Validator classes
#'
#' Abstract class for the Validator classes
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{getName()}}{Method that returns the name of the validator.}
#' }
#'
#' @return Character string indicating the name of the Validator class.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Validation Classes
#' @export
Validator0 <- R6::R6Class(
  "Validator0",
  private = list(
    ..name = 'Validator0'
  ),
  public = list(
    initialize = function() stop("The initialization method is not implemented for this abstract class."),
    getName = function() private$..name
  )
)

