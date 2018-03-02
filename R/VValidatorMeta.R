#==============================================================================#
#                              VValidatorMeta                                  #
#==============================================================================#
#' VValidatorMeta
#'
#'
#' \code{VValidatorMeta} Visitor class for validating Meta data objects.
#'
#' @section VValidatorMeta Methods:
#'  \describe{
#'   \item{\code{meta(object)}}{Method for validating a Meta class object.}
#' }
#'
#' @param object Object of the class for which the meta data is being created.
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VValidatorMeta <- R6::R6Class(
  classname = "VValidatorMeta",
  inherit = VValidator,
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    validate = function(object) {

      status <- list()
      status[['code']] <- TRUE
      
      # Get parameters
      p <- object$getParams()
      key <- p$key
      value <- p$value

      if (!(private$..request$className %in% private$..classes)) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0('Unable to add ', private$..request$field,
                                  ". Only character and numeric classes ",
                                  "are supported. ")
      }
      return(status)
    }
  ),

  public = list(

    initialize = function(object, request) {

      status <- list()
      status[['code']] <- TRUE
      if(missing(object)) {

        status[['code']] <- FALSE
        status[['msg']] <- paste0("Object parameter missing with no default. ")
        return(status)
      }
      if(missing(request)) {

        status[['code']] <- FALSE
        status[['msg']] <- paste0("Request parameter missing with no default. ")
        return(status)
      }

      private$..request <- request

      invisible(self)
    },

    pipeline = function(object) {
      return(private$validate(object))
    },

    corpus = function(object) {
      return(private$validate(object))
    },

    document = function(object) {
      return(private$validate(object))
    },

    model = function(object) {
      return(private$validate(object))
    }
  )
)
