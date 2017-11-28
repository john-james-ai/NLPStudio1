#==============================================================================#
#                            VValidatorRemoveChild                             #
#==============================================================================#
#' VValidatorRemoveChild
#'
#'
#' \code{VValidatorRemoveChild} Visitor class responsible for validating the parameters for the removeChild methods
#' of aggregate and composite classes.
#'
#' \strong{VValidatorRemoveChild Methods:}
#' The VValidatorRemoveChild methods are as follows:
#'  \itemize{
#'   \item{\code{nlpStudio(object, child)}}{Method for validating the removeChild method parameters of the NLPStudio object}
#'   \item{\code{lab(object, child)}}{Method for validating the removeChild method parameters of the Lab object}
#'   \item{\code{documentCollection(object, child)}}{Method for validating the removeChild method parameters of the DocumentCollection object.}
#'   \item{\code{documentText(object, child)}}{Not implemented for this class.}
#'   \item{\code{documentCsv(object, child)}}{Not implemented for this class.}
#'   \item{\code{documentRdata(object, child)}}{Not implemented for this class.}
#'   \item{\code{documentXlsx(object, child)}}{Not implemented for this class.}
#' }
#'
#' @param object The parent object
#' @param child  The child object
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VValidatorRemoveChild <- R6::R6Class(
  classname = "VValidatorRemoveChild",
  inherit = VValidator,
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    ..parent = character(0),
    ..child = character(0),

    validate = function(classes, object) {

      status <- list()
      status[['code']] <- TRUE

      # Confirm parent and visitor acceptor are a match
      if (private$..parent$getName() != object$getName()) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Parent and visitor acceptor mismatch. ",
                              "See ?", class(self)[1], " for further assistance.")
        return(status)
      }

      # Confirm class of child
      v <- ValidatorClass$new()
      if (v$validate(value = private$..child, expect = classes) == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Unable to remove ", class(private$..child)[1],
                                  " class object from an object of class ",
                                  class(private$..parent)[1], ".",
                                 "See ?", class(private$..parent)[1],
                                 " for further assistance.")
        return(status)
      }
      return(status)
    },

    validateDocument = function(object) {

      status <- list()
      status[['code']] <- FALSE
      status[['msg']] <- paste0("Unable to remove children to objects of the ",
                            class(private$..parent)[1], " class. ",
                            "See ?", class(private$..parent)[1], " for further assistance.")
      return(status)
    }
  ),

  public = list(

    initialize = function(parent, child) {

      status <- list()
      status[['code']] <- TRUE
      if(missing(parent)) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Parent parameter missing with no default. ")
        return(status)
      }
      if(missing(child)) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Child parameter missing with no default. ")
        return(status)
      }

      private$..parent <- parent
      private$..child <- child

      invisible(self)
    },

    nlpStudio = function(object) {
      classes <- "Lab"
      return(private$validate(classes, object))
    },

    lab = function(object) {
      classes <- "DocumentCollection"
      return(private$validate(classes, object))
    },

    documentCollection = function(object) {
      classes <- c("DocumentCollection", "DocumentText", "DocumentCsv",
                   "DocumentRdata", "DocumentXlsx")
      return(private$validate(classes, object))
    },

    documentText = function(object) {
      return(private$validateDocument(object))
    },

    documentCsv = function(object) {
      return(private$validateDocument(object))
    },

    documentRdata = function(object) {
      return(private$validateDocument(object))
    },

    documentXlsx = function(object) {
      return(private$validateDocument(object))
    }
  )
)
