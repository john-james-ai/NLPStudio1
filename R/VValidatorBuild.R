#==============================================================================#
#                             VValidatorBuild                                  #
#==============================================================================#
#' VValidatorBuild
#'
#'
#' \code{VValidatorBuild} Visitor class responsible for validating the objects prior to build operation
#'
#' \strong{VValidatorBuild Methods:}
#' The VValidatorBuild methods are as follows:
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating the validator visitor.}
#'   \item{\code{pipeline(object)}}{Method for validating the Pipeline object prior to a build operation.}
#'   \item{\code{corpus(object)}}{Method for validating the Corpus object prior to a build operation.}
#'   \item{\code{model(object)}}{Method for validating the Model object prior to a build operation.}
#' }
#'
#' @param object The object to be built
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VValidatorBuild <- R6::R6Class(
  classname = "VValidatorBuild",
  inherit = VValidator,
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..name = "VValidatorBuild",

    validateFileType = function(object) {

      status <- list()
      status[['code']] <- TRUE

      path <- object$getPattern()

      if (!is.null(path) & !isDirectory(path)) {

        type <- tolower(tools::file_ext(path))

        if (!(type %in% c('txt', 'csv', 'rdata', 'rds'))) {
          status[['code']] <- FALSE
          status[['msg']] <- paste0("Unable to build ", class(object)[1],
                                    " object. File type, ", type,
                                    " is not supported. ",
                                    "See ?", class(object)[1],
                                    " for further assistance")
          return(status)
        }
      }
      return(status)
    },

    validateStub = function(object) {
      status <- list()
      status[['code']] <- TRUE
      return(status)
    }
  ),

  public = list(

    initialize = function() {
      invisible(self)
    },

    pipeline = function(object) {
      return(private$validateStub(object))
    },

    corpus = function(object) {
      return(private$validateFileType(object))
    },

    model = function(object) {
      return(private$validateStub(object))
    }
  )
)
