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

      if (!("character" %in% class(value)))  return(FALSE)
      if (!exists('value')) return(FALSE)
      if (length(value) != 1) return(FALSE)
      if (is.na(value)) return(FALSE)
      if (is.null(value)) return(FALSE)
      if (is.logical(value)) return(FALSE)
      if (value == "") return(FALSE)
      if (grepl("\\s+", value) == TRUE) return(FALSE)
      return(TRUE)
    }
  )
)

