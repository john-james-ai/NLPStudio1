#==============================================================================#
#                                   VValidatorSetParent                        #
#==============================================================================#
#' VValidatorSetParent
#'
#'
#' \code{VValidatorSetParent} Visitor class responsible for validating the parameters for the SetParent methods
#' of aggregate and composite classes.
#'
#' \strong{VValidatorSetParent Methods:}
#' The VValidatorSetParent methods are as follows:
#'  \itemize{
#'   \item{\code{lab(object, parent)}}{Method for validating the SetParent method parameters of the Lab object}
#'   \item{\code{documentCollection(object, parent)}}{Method for validating the SetParent method parameters of the DocumentCollection object.}
#'   \item{\code{documentText(object, parent)}}{Method for validating the SetParent method parameters of the DocumentText object.}
#'   \item{\code{documentCsv(object, parent)}}{Method for validating the SetParent method parameters of the DocumentCsv object.}
#'   \item{\code{documentRdata(object, parent)}}{Method for validating the SetParent method parameters of the DocumentRdata object.}
#'   \item{\code{documentXlsx(object, parent)}}{Method for validating the SetParent method parameters of the DocumentXlsx object.}
#' }
#'
#' @param object The target object
#' @param parent  The parent object
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VValidatorSetParent <- R6::R6Class(
  classname = "VValidatorSetParent",
  inherit = VValidator,
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    ..name = 'VValidatorSetParent',
    ..object = character(0),
    ..parent = character(0),

    validate = function(classes, object) {

      status <- list()
      status[['code']] <- TRUE

      # If setting parent to NULL, return TRUE
      if (is.null(private$..parent)) {
        return(status)
      }

      # Confirm object and acceptor are a match
      if (private$..object$getName() != object$getName()) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Object and visitor acceptor mismatch. ",
                              "See ?", class(self)[1], " for further assistance.")
        return(status)
      }

      # Confirm class of parent
      v <- ValidatorClass$new()
      if (v$validate(value = private$..parent, expect = classes) == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Unable to set parent to object of ",
                                  class(private$..parent)[1], ".",
                                 "See ?", class(private$..object)[1],
                                 " for further assistance.")
        return(status)
      }
      return(status)
    }
  ),
  public = list(

    initialize = function(object, parent) {

      status <- list()
      status[['code']] <- TRUE
      if(missing(object)) {

        status[['code']] <- FALSE
        status[['msg']] <- paste0("Object parameter missing with no default. ")
        return(status)
      }
      if(missing(parent)) {

        status[['code']] <- FALSE
        status[['msg']] <- paste0("Parent parameter missing with no default. ")
        return(status)
      }

      private$..object <- object
      private$..parent <- parent

      invisible(self)
    },

    nlpStudio = function(object) {
      status <- list()
      status[['code']] <- FALSE
      status[['msg']] <-  "Unable to set parent to an NLPStudio object"
      return(status)
    },

    lab = function(object) {
      classes <- "NLPStudio"
      return(private$validate(classes, object))
    },

    documentCollection = function(object) {
      classes <- c("DocumentCollection", "Lab")
      return(private$validate(classes, object))
    },

    documentText = function(object) {
      classes <- c("DocumentCollection")
      return(private$validate(classes, object))
    },

    documentCsv = function(object) {
      classes <- c("DocumentCollection")
      return(private$validate(classes, object))
    },

    documentRdata = function(object) {
      classes <- c("DocumentCollection")
      return(private$validate(classes, object))
    },

    documentXlsx = function(object) {
      classes <- c("DocumentCollection")
      return(private$validate(classes, object))
    }
  )
)
