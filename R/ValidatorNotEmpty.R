## ---- ValidatorNotEmpty
#==============================================================================#
#                             ValidatorNotEmpty                                 #
#==============================================================================#
#' ValidatorNotEmpty
#'
#'
#' \code{ValidatorNotEmpty} Class for validating string is not empty
#'
#' This class provide a methods for validating character string is not empty
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{validate(value, expect = NULL)}}{Method for validating string}
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
ValidatorNotEmpty <- R6::R6Class(
  "ValidatorNotEmpty",
  inherit = Validator0,
  private = list(
    ..name = 'ValidatorNotEmpty'
  ),
  public = list(
    validate = function(value, expect = NULL) {

      if (length(value) == 0) {
        return(FALSE)
      } else if (length(value) == 1) {
        if (value == "") {
          return(FALSE)
        } else {
          return(TRUE)
        }
      } else {
        return(TRUE)
      }
    }
  )
)
