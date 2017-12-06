## ---- ValidatorUrl
#==============================================================================#
#                               ValidatorUrl                                    #
#==============================================================================#
#' ValidatorUrl
#'
#'
#' \code{ValidatorUrl} Class for validating website urls
#'
#' This class provide a methods for validating website urls
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Creates an object of ValidatorUrl class}
#'  \item{\code{validate(object)}}{Method for validating urls}
#' }
#'
#' @return A logical TRUE if valid url, FALSE otherwise
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Validation Classes
#' @export
ValidatorUrl <- R6::R6Class(
  "ValidatorUrl",
  private = list(
    ..name = 'ValidatorUrl'
  ),
  public = list(
    validate = function(value, expect = NULL) {
      if (exists('value') & length(value) != 0) {
        if (!RCurl::url.exists(value)) {
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

