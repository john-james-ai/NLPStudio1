#==============================================================================#
#                           VValidatorCorpusBuilder                            #
#==============================================================================#
#' VValidatorCorpusBuilder
#'
#'
#' \code{VValidatorCorpusBuilder} Visitor class responsible for validating the initialization CorpusBuilder objects
#'
#' @section VValidatorCorpusBuilder Methods:
#'  \describe{
#'   \item{corpusBuilderRawWeb}{Concrete builder sub-class that produces the Raw Corpus object. }
#'   \item{corpusBuilderRefined}{Concrete builder sub-class that produces the Refined Corpus object. }
#'   \item{corpusBuilderReshaped}{Concrete builder sub-class that produces the Reshaped Corpus object. }
#'   }
#'
#' @param object The object in its current state
#' @param ... Parameters
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Classes
#' @export
VValidatorCorpusBuilder <- R6::R6Class(
  classname = "VValidatorCorpusBuilder",
  inherit = VValidator0,
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    ..name = "VValidatorCorpusBuilder",

    validateInputUrl = function(builder) {

      status <- list()
      status[['code']] <- TRUE

      url <- builder$getInput()
      v <- ValidatorUrl$new()
      if (v$validate(value = url) == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Unable to create ", builder$getClassName(),
                                  " object for ", builder$getName(), ". ",
                                  "The URL is invalid.  See ?", builder$getClassName(),
                                  " for further assistance.")
      }
      return(status)
    },

    validateInputCorpus = function(builder) {

      status <- list()
      status[['code']] <- TRUE

      corpus <- builder$getInput()
      v <- ValidatorExists$new()
      if (v$validate(value = corpus) == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Unable to create ", builder$getClassName(),
                                  " object for ", builder$getName(), ". ",
                                  "The input Corpus object does not exist. ",
                                  "See ?", builder$getClassName(),
                                  " for further assistance. ")
      }
      return(status)
    }
  ),

  public = list(

    initialize = function() {
      invisible(self)
    },

    corpusBuilderRawWeb = function(builder) {
      return(private$..validateInputUrl(builder))
    },

    corpusBuilderRefined = function(builder) {
      return(private$..validateInputCorpus(builder))
    },

    corpusBuilderReshaped = function(builder) {
      return(private$..validateInputCorpus(builder))
    }
  )
)
