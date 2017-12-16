#==============================================================================#
#                                   VValidatorMeta                             #
#==============================================================================#
#' VValidatorMeta
#'
#'
#' \code{VValidatorMeta} Visitor class responsible for validating the assignment and removal of members from classes.
#'
#' @section VValidatorMeta Methods:
#'  \describe{
#'   \item{\code{pipeline(object, request)}}{Method for validating a request to add a member to the Pipeline class.}
#'   \item{\code{corpus(object, request)}}{Method for validating a request to add a member to the Corpus class.}
#'   \item{\code{document(object, request)}}{Method for validating a request to add a member to the Document class.}
#' }
#'
#' @param object Object of the class for which the meta data is being created.
#' @param request  List containing the field, its classname
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

    ..name = 'VValidatorMeta',
    ..classes = c('character()', 'numeric()'),
    ..request = list(),

    validate = function(object) {

      status <- list()
      status[['code']] <- TRUE

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
