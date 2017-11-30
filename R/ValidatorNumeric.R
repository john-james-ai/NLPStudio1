## ---- ValidatorNumeric
#==============================================================================#
#                             ValidatorNumeric                                  #
#==============================================================================#
#' ValidatorNumeric
#'
#'
#' \code{ValidatorNumeric} Class for validating numerics
#'
#' This class provide a methods for validating numerics.
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{validate(value, expect = NULL)}}{Method for validating numeric}
#' }
#'
#' @section Parameters:
#' @param value Character string containing the value to be validated
#' @param expect Character string or logical indicating the expected value
#'
#' @return A logical TRUE if class is equal to expected value, FALSE otherwise.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Validation Classes
#' @export
ValidatorNumeric <- R6::R6Class(
  "ValidatorNumeric",
  inherit = Validator0,
  private = list(
    ..name = 'ValidatorNumeric'
  ),
  public = list(
    validate = function(value, expect = NULL) {

      if (exists('value') & length(value) != 0) {
        if (!is.numeric(value)) {
          return(FALSE)
        } else {
          return(TRUE)
        }
      } else {
        return(FALSE)
      }
    }
  )
)

