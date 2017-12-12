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
#'   \item{\code{studio(object, parent)}}{Method for validating the SetParent method parameters of the Studio object}
#'   \item{\code{documentCollection(object, parent)}}{Method for validating the SetParent method parameters of the DocumentCollection object.}
#'   \item{\code{document(object, parent)}}{Method for validating the SetParent method parameters of the Document object.}
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

    nlpStudios = function(object) {
      status <- list()
      status[['code']] <- FALSE
      status[['msg']] <-  "Unable to set parent to an NLPStudios object"
      return(status)
    },

    studio = function(object) {
      classes <- c("NLPStudios", "Archive")
      return(private$validate(classes, object))
    },

    pipeline = function(object) {
      classes <- c("Studio", "Archive")
      return(private$validate(classes, object))
    },

    data = function(object) {
      classes <- c("Pipeline", "Archive")
      return(private$validate(classes, object))
    },

    feature = function(object) {
      classes <- c("Pipeline", "Archive")
      return(private$validate(classes, object))
    },

    model = function(object) {
      classes <- c("Pipeline", "Archive")
      return(private$validate(classes, object))
    },

    corpus = function(object) {
      classes <- c("Data", "Archive")
      return(private$validate(classes, object))
    },

    document = function(object) {
      classes <- c("Corpus", "Archive")
      return(private$validate(classes, object))
    },

    featureNGram = function(object) {
      classes <- c("Pipeline", "Archive")
      return(private$validate(classes, object))
    },

    featurePOS = function(object) {
      classes <- c("Pipeline", "Archive")
      return(private$validate(classes, object))
    }
  )
)
