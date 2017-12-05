## ---- ValidatorClass
#==============================================================================#
#                               ValidatorClass                                  #
#==============================================================================#
#' ValidatorClass
#'
#'
#' \code{ValidatorClass} Class for validating object class
#'
#' This class provide a methods for validating an object's class.
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{validate(value, expect = NULL)}}{Method for validating logical}
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
ValidatorClass <- R6::R6Class(
  "ValidatorClass",
  inherit = Validator0,
  private = list(
    ..name = 'ValidatorClass'
  ),
  public = list(
    validate = function(value, expect = NULL) {
      if (exists('value') & length(value) != 0) {
        classes <- class(value)
        if (length(intersect(expect, classes)) > 0) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      } else {
        return(FALSE)
      }
    }
  )
)

