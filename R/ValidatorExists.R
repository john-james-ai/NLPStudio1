## ---- ValidatorExists
#==============================================================================#
#                             ValidatorExists                                   #
#==============================================================================#
#' ValidatorExists
#'
#'
#' \code{ValidatorExists} Class for validating whether an object exists
#'
#' This class provide a methods for validating the existence of objects in
#' the global environment.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{validate(value, expect = NULL)}}{Method for validating variable existence}
#' }
#'
#' @section Parameters:
#' @param value Character string containing the value to be validated
#' @param expect Character string or logical indicating the expected value
#'
#' @return A logical TRUE if class is equal to expected value, FALSE otherwise.
#'
#' @docType class
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Validation Classes
#' @export
ValidatorExists <- R6::R6Class(
  "ValidatorExists",
  inherit = Validator0,
  private = list(
    ..name = 'ValidatorExists'
  ),
  public = list(
    initialize = function() invisible(self),
    validate = function( value, expect = TRUE) {
      if (length(value) != 0 & !is.null(value)) {
        if (exists(value) & (expect == TRUE | expect == "TRUE")) {
          return(TRUE)
        } else if (!exists(value) & (expect == FALSE | expect == "FALSE")){
          return(TRUE)
        } else {
          return(FALSE)
        }
      }
    }
  )
)
