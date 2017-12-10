## ---- ValidatorIO
#==============================================================================#
#                             ValidatorIO                                      #
#==============================================================================#
#' ValidatorIO
#'
#' \code{ValidatorIO} Class for validating the io method for an object.
#'
#' Class compares the file type with the IO Class assigned to the object and
#' returns FALSE if there is a mismatch.
#'
#' @docType class
#'
#' @section Parameters:
#' @param value Character string containing the value to be validated
#'
#' @return A logical TRUE if class is equal to expected value, FALSE otherwise.
#'
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Validation Classes
#' @export
ValidatorIO <- R6::R6Class(
  "ValidatorIO",
  inherit = Validator0,
  private = list(
    ..name = 'ValidatorIO'
  ),
  public = list(
    initialize = function() invisible(self),
    validate = function(value) {

      fileType <- tools::file_ext(value$getFileName())
      io <- value$getIO()

      if (fileType == 'txt' & isTRUE(all.equal(io, IOText$new()))) return(TRUE)
      if (fileType %in% c('Rdata', 'RData') & isTRUE(all.equal(io, IOText$new()))) return(TRUE)
      if (fileType == 'csv' & isTRUE(all.equal(io, IOCSV$new()))) return(TRUE)
      if (fileType == 'xlsx' & isTRUE(all.equal(io, IOXlsx$new()))) return(TRUE)
      return(FALSE)
    }
  )
)
