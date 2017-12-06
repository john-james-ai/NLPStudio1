#==============================================================================#
#                           VValidatorKorpusBuilder                            #
#==============================================================================#
#' VValidatorKorpusBuilder
#'
#'
#' \code{VValidatorKorpusBuilder} Visitor class responsible for validating the initialization objects of all classes
#'
#' \strong{VValidatorKorpusBuilder Methods:}
#' The VValidatorKorpusBuilder methods are as follows:
#'  \itemize{
#'   \item{\code{nlpStudio(object)}}{Method for validating the instantiation of the NLPStudio object}
#'   \item{\code{lab(object)}}{Method for validating the instantiation of the Lab object}
#'   \item{\code{documentCollection(object)}}{Method for validating the instantiation of the DocumentCollection object.}
#'   \item{\code{documentText(object)}}{Method for validating the instantiation of the DocumentText object.}
#'   \item{\code{documentCsv(object)}}{Method for validating the instantiation of the DocumentCsv object.}
#'   \item{\code{documentRdata(object)}}{Method for validating the instantiation of the DocumentRdata object.}
#'   \item{\code{documentXlsx(object)}}{Method for validating the instantiation of the DocumentXlsx object.}
#' }
#'
#' @param object The object in its current state
#' @param ... Parameters
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Classes
#' @export
ValidatorKorpusBuilder <- R6::R6Class(
  classname = "ValidatorKorpusBuilder",
  inherit = Validator0,
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    ..name = "ValidatorKorpusBuilder",

    validateName = function(object) {

      status <- list()
      status[['code']] <- TRUE

      name <- object$getName()

      # Confirm not missing
      if (is.null(name) | is.na(name)) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Name parameter is missing with no default. ",
                              "See ?", class(object)[1], " for further assistance.")
        return(status)
      }

      # Confirm object doesn't already exist.
      v <- ValidatorExists$new()
      if (v$validate(value = name, expect = FALSE) == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Cannot create ", class(object)[1],
                                 " object. ", name, " already exists. ",
                                 "See ?", class(object)[1],
                                 " for further assistance")
        return(status)
      }

      # Validate name is well-formed
      v <- ValidatorString$new()
      if (v$validate(value = name, expect = NULL) == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Cannot create ", class(object)[1],
                                  " object. ", name, " must be a character ",
                                  "string.  See ?", class(object)[1],
                                  " for further assistance")
        return(status)
      }
      return(status)
    },

    validateURL = function(object) {

      status <- list()
      status[['code']] <- TRUE

      url <- object$url
      v <- ValidatorUrl$new()
      if (v$validate(value = url) == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste("URL", url, "is invalid.")
      }
      return(status)
    },

    validateExist = function(object, thing) {

      status <- list()
      status[['code']] <- TRUE

      if (missing(object)) {
        status[['code']] <- FALSE
        status[['msg']] <- paste(thing, "parameter is missing with no default.")
      }
      return(status)
    },

    validateSplits = function(object) {

      status <- list()
      status[['code']] <- TRUE

      if (sum(object$splits) > 1) {
        status[['code']] <- FALSE
        status[['msg']] <- paste("Splits exceed 100% of the file size.")
      }
      return(status)
    },

    validateNumeric = function(object, thing) {

      status <- list()
      status[['code']] <- TRUE

      v <- ValidatorNumeric$new()
      if (v$validate(object$nGrams) == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste(thing, "parameter is required and must be numeric.")
      }
      return(status)
    }
  ),

  public = list(

    initialize = function() {
      invisible(self)
    },

    getData = function(object) {
        return(private$validateURL(object))
    },

    buildDocuments = function(object) {
      return(status[['code']] <- TRUE)
    },

    repairDocuments = function(object) {
      return(private$validateExist(object$repairs, 'Repair'))
    },

    splitDocuments = function(object) {
      return(private$validateSplits(object))
    },

    sampleDocuments = function(object) {
      return(status[['code']] <- TRUE)
    },

    normalizeDocuments = function(object) {
      return(private$validateExist(object$normalize, 'Normalize'))
    },

    correctDocuments = function(object) {
      return(private$validateExist(object$corrections, 'Corrections'))
    },

    profanity = function(object) {
      return(private$validateExist(object$profanity, 'Profanity'))
    },

    nGramDocuments = function(object) {
      return(private$validateNumeric(object$nGrams, 'NGram'))
    },

    posTags = function(object) {
      return(status[['code']] == TRUE)
    }
  )
)
