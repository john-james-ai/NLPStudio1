## ---- ValidatorNoSpaces
#==============================================================================#
#                             ValidatorNoSpaces                                 #
#==============================================================================#
#' ValidatorNoSpaces
#'
#'
#' \code{ValidatorNoSpaces} Class for validating string has no spaces
#'
#' This class provide a methods for validating character string has no spaces
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{validate(value, expect = NULL)}}{Method for validating string}
#' }
#'
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
ValidatorNoSpaces <- R6::R6Class(
  "ValidatorNoSpaces",
  inherit = Validator0,
  private = list(
    ..name = 'ValidatorNoSpaces'
  ),
  public = list(
    validate = function(value, expect = NULL) {

      if (exists('value')) {
        if (length(value) > 0) {
          if (grepl(pattern = "^\\S+\\s+", x = value, perl = TRUE)) {
            return(FALSE)
          } else {
            return(TRUE)
          }
        } else {
          return(FALSE)
        }
      } else {
        return(FALSE)
      }
    }
  )
)

