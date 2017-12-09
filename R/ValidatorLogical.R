## ---- ValidatorLogical
#==============================================================================#
#                             ValidatorLogical                                  #
#==============================================================================#
#' ValidatorLogical
#'
#'
#' \code{ValidatorLogical} Class for validating logicals
#'
#' This class provide a methods for validating logicals.
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
ValidatorLogical <- R6::R6Class(
  "ValidatorLogical",
  inherit = Validator0,
  private = list(
    ..name = 'ValidatorLogical'
  ),
  public = list(
    initialize = function() invisible(self),
    validate = function(value, expect = NULL) {
        if (is.logical(value) | value == "TRUE" | value == "FALSE") {
          return(TRUE)
        } else {
          return(FALSE)
        }
    }
  )
)

