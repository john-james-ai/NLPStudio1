#==============================================================================#
#                                   VValidatorAddChild                             #
#==============================================================================#
#' VValidatorAddChild
#'
#'
#' \code{VValidatorAddChild} Visitor class responsible for validating the parameters for the addChild methods
#' of aggregate and composite classes.
#'
#' \strong{VValidatorAddChild Methods:}
#' The VValidatorAddChild methods are as follows:
#'  \itemize{
#'   \item{\code{nlpStudio(object, child)}}{Method for validating the addChild method parameters of the NLPStudio object}
#'   \item{\code{lab(object, child)}}{Method for validating the addChild method parameters of the Lab object}
#'   \item{\code{documentCollection(object, child)}}{Method for validating the addChild method parameters of the DocumentCollection object.}
#'   \item{\code{documentText(object, child)}}{Method for validating the addChild method parameters of the DocumentText object.}
#'   \item{\code{documentCsv(object, child)}}{Method for validating the addChild method parameters of the DocumentCsv object.}
#'   \item{\code{documentRdata(object, child)}}{Method for validating the addChild method parameters of the DocumentRdata object.}
#'   \item{\code{documentXlsx(object, child)}}{Method for validating the addChild method parameters of the DocumentXlsx object.}
#' }
#'
#' @param object The parent object
#' @param child  The child object
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VValidatorAddChild <- R6::R6Class(
  classname = "VValidatorAddChild",
  inherit = VValidator,
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    ..name = "VValidatorAddChild",
    ..parent = character(0),
    ..child = character(0),

    validate = function(classes, object) {
      status <- list()
      status[['code']] <- TRUE

      # Confirm object and parent are a match
      if (private$..parent$getName() != object$getName()) {
        status[['code']] <- FALSE
        status[['msg']] < paste0("Parent and visitor acceptor mismatch. ",
                                 "See ?", class(self)[1], " for further assistance.")
        return(status)
      }

      # Confirm class of child
      v <- ValidatorClass$new()
      if (v$validate(value = private$..child, expect = classes) == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Unable to add ", class(private$..child)[1],
                                  " class object to an object of class ",
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
      status[['msg']] <- paste0("Unable to add children to objects of the ",
                                class(private$..parent)[1], " class. ",
                                "See ?", class(private$..parent)[1],
                                " for further assistance.")
      return(status)
    }
  ),

  public = list(

    initialize = function(parent, child) {

      status <- list()

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
<<<<<<< HEAD
      classes <- "Korpus"
      return(private$validate(classes, object))
    },

    korpus = function(object) {
      classes <- c("DocumentText", "DocumentCsv",
=======
      classes <- "DocumentCollection"
      return(private$validate(classes, object))
    },

    documentCollection = function(object) {
      classes <- c("DocumentCollection", "DocumentText", "DocumentCsv",
>>>>>>> e259b2c7c12e4427e6348465304e1cdb73f2a900
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
