#==============================================================================#
#                         VValidatorCorpusSourceWeb                            #
#==============================================================================#
#' VValidatorCorpusSourceWeb
#'
#'
#' \code{VValidatorCorpusSourceWeb} Visitor class responsible for validating the
#' parameters for the CorpusSourceWeb method in the Corpus Object.
#'
#' @section Corpus Core Methods:
#'  \describe{
#'   \item{\code{new()}}{Instantiates this visitor object. }
#'   \item{\code{object, source}}{The method that performs the validation of the corpus object.}
#'  }
#'
#' @param object The Corpus object
#' @param source Character string containing url from which the Corpus object will be sourced.
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VValidatorCorpusSourceWeb <- R6::R6Class(
  classname = "VValidatorCorpusSourceWeb",
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
      if (v$validate(value = object, expect = c('Corpus', 'CorpusSourceWeb')) == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Unable to source ", class(object)[1],
                                  " class object. See ?", class(object)[1],
                                 " for further assistance.")
        return(status)
      }

      # Confirm class of source
      v <- ValidatorClass$new()
      if (v$validate(value = private$..source, expect = 'character') == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Unable to source ", class(object)[1],
                                  " class object, ", object$getName(),
                                  ". Source is not of type character. ",
                                  "See ?", class(object)[1],
                                  " for further assistance.")
        return(status)
      }

      # Confirm URL exists
      url <- private$..source
      v <- ValidatorUrl$new()
      if (v$validate(value = url) == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Unable to source ", object$getClassName(),
                                  " object, ", object$getName(), ". ",
                                  "The URL is invalid. ",
                                  "See ?", object$getClassName(),
                                  " for further assistance. ")
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

    corpusSourceWeb = function(object) {
      return(private$validate(object))
    }
  )
)
