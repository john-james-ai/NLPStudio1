## ---- ValidatorString
#==============================================================================#
#                             ValidatorString                                   #
#==============================================================================#
#' ValidatorString
#'
#'
#' \code{ValidatorString} Class for validating strings
#'
#' This class provide a methods for validating character strings
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
ValidatorString <- R6::R6Class(
  "ValidatorString",
  inherit = Validator0,
  private = list(
    ..name = 'ValidatorString'
  ),
  public = list(
    initialize = function() invisible(self),
    validate = function(value, expect = NULL) {

      if (exists('value') & length(value) != 0) {
        if (is.na(value) | is.null(value) | !is.character(value)
            | is.logical(value) | value == "" ) {
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

