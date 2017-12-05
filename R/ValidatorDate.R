## ---- ValidatorDate
#==============================================================================#
#                               ValidatorDate                                  #
#==============================================================================#
#' ValidatorDate
#'
#'
#' \code{ValidatorDate} Class for validating and parsing dates
#'
#' This class provide a methods for validating and parsing dates. This class
#' uses the 'parsedate' package to return a POSIXct object for any
#' recognizable date format.
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{validate(value, expect = NULL)}}{Method for validating and parsing dates}
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
ValidatorDate <- R6::R6Class(
  "ValidatorDate",
  inherit = Validator0,
  private = list(
    ..name = 'ValidatorDate'
  ),
  public = list(
    validate = function(value, expect = NULL) {

      date <- parsedate::parse_date(value, approx = TRUE)

      if (is.na(date)) {
        return(FALSE)
      } else {
        return(date)
      }
    }
  )
)

