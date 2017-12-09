#==============================================================================#
#                         VValidatorCorpusInSource                             #
#==============================================================================#
#' VValidatorCorpusInSource
#'
#'
#' \code{VValidatorCorpusInSource} Visitor class responsible for validating the
#' parameters for the CorpusInSource method in the Corpus Object.
#'
#' @section Corpus Core Methods:
#'  \describe{
#'   \item{\code{new()}}{Instantiates this visitor object. }
#'   \item{\code{object, source}}{The method that performs the validation of the corpus object.}
#'  }
#'
#' @param object The Corpus object
#' @param source Corpus object from which the object will be sourced
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VValidatorCorpusInSource <- R6::R6Class(
  classname = "VValidatorCorpusInSource",
  inherit = VValidator,
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    ..object = character(),
    ..source = character(),

    validate = function(object) {
      status <- list()
      status[['code']] <- TRUE

      # Confirm object in initialization and validation are a match
      if (private$..object$getName() != object$getName()) {
        status[['code']] <- FALSE
        status[['msg']] < paste0("Object and visitor acceptor mismatch. ",
                                 "See ?", class(self)[1], " for further assistance.")
        return(status)
      }

      # Confirm class of object
      v <- ValidatorClass$new()
      if (v$validate(value = object, expect = 'Corpus') == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Unable to source ", class(object)[1],
                                  " class object. See ?", class(object)[1],
                                  " for further assistance.")
        return(status)
      }

      # Confirm class of source
      v <- ValidatorClass$new()
      if (v$validate(value = private$..source, expect = 'Corpus') == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Unable to source ", class(object)[1],
                                  " class object, ", object$getName(),
                                  ". Source is a valid Corpus object. ",
                                  "See ?", class(object)[1],
                                  " for further assistance.")
        return(status)
      }
      return(status)
    }
  ),

  public = list(

    initialize = function(object, source) {

      status <- list()

      if(missing(object)) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Corpus parameter missing with no default. ")
        return(status)
      }
      if(missing(source)) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Source parameter missing with no default. ")
        return(status)
      }

      private$..object <- object
      private$..source <- source

      invisible(self)
    },

    corpus = function(object) {
      return(private$validate(object))
    }
  )
)
