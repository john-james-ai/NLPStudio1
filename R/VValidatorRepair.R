#==============================================================================#
#                              VValidatorRepair                                  #
#==============================================================================#
#' VValidatorRepair
#'
#'
#' \code{VValidatorRepair} Visitor class responsible for validating read
#' requests.
#'
#' \strong{VValidatorRepair Methods:}
#' The VValidatorRepair methods are as follows:
#'  \itemize{
#'   \item{\code{documentCollection(object)}}{Method for validating a request ro read a DocumentCollection object.}
#'   \item{\code{documentText(object)}}{Method for validating a request ro read a DocumentText object.}
#'   \item{\code{documentCsv(object)}}{Method for validating a request ro read a DocumentCsv object.}
#'   \item{\code{documentRdata(object)}}{Method for validating a request ro read a DocumentRdata object.}
#'   \item{\code{documentXlsx(object)}}{Method for validating a request ro read a DocumentXlsx object.}
#' }
#'
#' @param object The document class object
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VValidatorRepair <- R6::R6Class(
  classname = "VValidatorRepair",
  inherit = VValidator,
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    validate = function(object) {

      d <- object$getDocument()

      # Validate path
      v <- Validator$new()
      if (is.null(d$path) | !dir.exists(d$path)) {
        v$notify(class = class(self)[1], method = "validate",
                 fieldName = "path", value = "",
                 level = "Error",
                 msg = paste0("Unable to read document. ",
                              "Document path is invalid or missing with no default.",
                              " See ?", class(object)[1],
                              " for further assistance."),
                 expect = NULL)
        return(FALSE)
      }

      # Validate filename
      if (missing(d$fileName)) {
        v$notify(class = class(self)[1], method = "validate",
                 fieldName = "fileName", value = "",
                 level = "Error",
                 msg = paste0("Unable to read document. ",
                              "Document file name is  missing with no default.",
                              " See ?", class(object)[1],
                              " for further assistance."),
                 expect = NULL)
        return(FALSE)
      }

      # Validate full path
      f <- file.path(d$path, d$fileName)
      if (!file.exists(f)) {
        v$notify(class = class(self)[1], method = "validate",
                 fieldName = "fileName", value = f,
                 level = "Error",
                 msg = paste0("Unable to read document. ",
                              "Invalid file path or file name.",
                              " See ?", class(object)[1],
                              " for further assistance."),
                 expect = NULL)
        return(FALSE)
      }
      return(TRUE)
    }
  ),

  public = list(

    documentCollection = function(object) stop("The repair method is not implemented for this class"),

    documentText = function(object) {
      return(private$validate(object))
    },

    documentCsv = function(object) stop("The repair method is not implemented for this class"),

    documentRdata = function(object) stop("The repair method is not implemented for this class"),

    documentXlsx = function(object) stop("The repair method is not implemented for this class")
  )
)
