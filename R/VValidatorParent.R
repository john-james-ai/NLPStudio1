#==============================================================================#
#                                   VValidatorParent                             #
#==============================================================================#
#' VValidatorParent
#'
#'
#' \code{VValidatorParent} Visitor class responsible for validating the parameters for the parent methods
#' of aggregate and composite classes.
#'
#' \strong{VValidatorParent Methods:}
#' The VValidatorParent methods are as follows:
#'  \itemize{
#'   \item{\code{nlpStudio(object, ...)}}{Method for validating the parent method parameters of the NLPStudio object}
#'   \item{\code{lab(object, ...)}}{Method for validating the parent method parameters of the Lab object}
#'   \item{\code{documentCollection(object, ...)}}{Method for validating the parent method parameters of the DocumentCollection object.}
#'   \item{\code{documentText(object, ...)}}{Method for validating the parent method parameters of the DocumentText object.}
#'   \item{\code{documentCsv(object, ...)}}{Method for validating the parent method parameters of the DocumentCsv object.}
#'   \item{\code{documentRdata(object, ...)}}{Method for validating the parent method parameters of the DocumentRdata object.}
#'   \item{\code{documentXlsx(object, ...)}}{Method for validating the parent method parameters of the DocumentXlsx object.}
#' }
#'
#' @param object The object in its current state
#' @param ... Parameters
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VValidatorParent <- R6::R6Class(
  classname = "VValidatorParent",
  inherit = VValidator,
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..name = 'VValidatorParent',
    validate = function(classes, object, ...) {

      status <- list()
      status[['code']] <- TRUE

      # Validate class of object
      if (class(object)[1] %in% c("NLPStudio", "Lab")) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("This method is not implemented for the ",
                                  class(object)[1], " class. ",
                                  "See ?", class(object)[1],
                                  " for further assistance.")
        return(status)
      }

      # Validate parent parameter
      if (missing(parent)) {
        return(status)
      } else {
        # Confirm class of child
        v <- ValidatorClass$new()
        if (v$validate(value = parent, expect = classes) == FALSE) {
          status[['code']] <- FALSE
          status[['msg']] <- paste0(class(parent)[1], " is an invalid parent",
                                    " class for object of the ", class(object)[1],
                                    " class. See ?", class(object)[1],
                                   " for further assistance.")
          return(status)
        }
      }
      return(status)
    }
  ),

  public = list(

    nlpStudio = function(object,...) {
      return(private$validate(classes = classes, object = object, ...))
    },

    lab = function(object,...) {
      return(private$validate(classes = classes, object = object, ...))
    },

    documentCollection = function(object,...) {
      classes <- c("Lab", "DocumentCollection")
      return(private$validate(classes = classes, object = object, ...))
    },

    documentText = function(object,...) {
      classes <- "DocumentCollection"
      return(private$validate(classes = classes, object = object, ...))
    },

    documentCsv = function(object,...) {
      classes <- "DocumentCollection"
      return(private$validate(classes = classes, object = object, ...))
    },

    documentRdata = function(object,...) {
      classes <- "DocumentCollection"
      return(private$validate(classes = classes, object = object, ...))
    },

    documentXlsx = function(object,...) {
      return(private$validate(classes = classes, object = object, ...))
    }
  )
)
