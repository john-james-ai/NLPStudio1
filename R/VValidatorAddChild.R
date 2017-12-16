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
#'   \item{\code{studio(object, child)}}{Method for validating the addChild method parameters of the Studio object}
#'   \item{\code{documentCollection(object, child)}}{Method for validating the addChild method parameters of the DocumentCollection object.}
#'   \item{\code{document(object, child)}}{Method for validating the addChild method parameters of the Document object.}
#'   \item{\code{documentCSV(object, child)}}{Method for validating the addChild method parameters of the DocumentCSV object.}
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
      classes <- "Pipeline"
      return(private$validate(classes, object))
    },

    pipeline = function(object) {
      classes <- c("Corpus", "CVSets")
      return(private$validate(classes, object))
    },

    corpus = function(object) {
      classes <- c("Document")
      return(private$validate(classes, object))
    },

    document = function(object) {
      return(private$validateDocument(object))
    },

    documentCSV = function(object) {
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
