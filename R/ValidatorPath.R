## ---- ValidatorPath
#==============================================================================#
#                             ValidatorPath                                     #
#==============================================================================#
#' ValidatorPath
#'
#' @description
#' \code{ValidatorPath} Class for validating a file path
#'
#' This class provide a methods for validating file paths.
#'
#' @docType class
#'
#' @section Parameters:
#' @param value Character string containing the value to be validated
#' @param expect Character string or logical indicating the expected value
#'
#' @return A logical TRUE if class is equal to expected value, FALSE otherwise.
#'
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Validation Classes
#' @export
ValidatorPath <- R6::R6Class(
  "ValidatorPath",
  public = list(

    validate = function(value, expect = NULL) {

      if (length(value) == 0) {
        return(FALSE)
      } else if (value == "") {
        return(FALSE)
      } else if (isDirectory(value) & !dir.exists(value) & expect == TRUE) {
        return(FALSE)
      } else if (isDirectory(value) & dir.exists(value) & expect == FALSE) {
        return(FALSE)
      } else if (!file.exists(value) & expect == TRUE) {
        return(FALSE)
      } else if (file.exists(value) & expect == FALSE) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
  )
)
