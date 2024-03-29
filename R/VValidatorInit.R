#==============================================================================#
#                                   VValidatorInit                             #
#==============================================================================#
#' VValidatorInit
#'
#'
#' \code{VValidatorInit} Visitor class responsible for validating the initialization objects of all classes
#'
#' \strong{VValidatorInit Methods:}
#' The VValidatorInit methods are as follows:
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
#' @family Validation Visitor Classes
#' @export
VValidatorInit <- R6::R6Class(
  classname = "VValidatorInit",
  inherit = VValidator,
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    ..name = "VValidatorInit",
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

    validateParent = function(object, parentClasses) {

      status <- list()
      status[['code']] <- TRUE

      parent <- object$parent
      name <- object$getName()

      v <- ValidatorClass$new()
      if (v$validate(value = parent, expect = parentClasses) == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Cannot create ", class(object)[1],
                                  " object, ", name, ". Object of class ",
                                  class(object)[1], " can not have an ",
                                  "object of class ", class(parent)[1],
                                  " as a parent. ",
                                  "See ?", class(object)[1],
                                  " for further assistance")
      }
      return(status)
    },

    validateState = function(object) {

      status <- list()
      status[['code']] <- TRUE

      o <- object$exposeObject()

      if (is.null(o$state) | is.na(o$state) | length(o$state) == 0) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("State element is missing with no default. ",
                                  "See ?", class(object)[1], " for further assistance.")
      }
      return(status)
    },


    validateFileName = function(object, ext) {

      status <- list()
      status[['code']] <- TRUE

      o <- object$exposeObject(self)

      if (is.null(o$fileName) | is.na(o$fileName) | length(o$fileName) == 0) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("File name parameter is missing with no default. ",
                                  "See ?", class(object)[1], " for further assistance.")
        return(status)
      }

      if (!(file_ext(o$fileName) %in% ext)) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("File type must be ", ext,
                                  "See ?", class(object)[1], " for further assistance.")
        return(status)
      }
      return(status)
    }
  ),

  public = list(

    initialize = function() {
      invisible(self)
    },

    nlpStudio = function(object) {
      return(status[['code']] <- TRUE)
    },

    lab = function(object) {
      return(private$validateName(object))
    },

    documentCollection = function(object) {
      return(private$validateName(object))
    },

    documentText = function(object) {

      if (private$validateName(object)[['code']] == FALSE)
        return(private$validateName(object))
      if (private$validateFileName(object, "txt")[['code']] == FALSE)
        return(private$validateFileName(object, "txt"))
      return(status[['code']] <- TRUE)
    },

    documentCsv = function(object) {

      if (private$validateName(object)[['code']] == FALSE)
        return(private$validateName(object))
      if (private$validateFileName(object, "csv")[['code']] == FALSE)
        return(private$validateFileName(object, "csv"))
      return(status[['code']] <- TRUE)
    },

    documentRdata = function(object) {

      if (private$validateName(object)[['code']] == FALSE)
        return(private$validateName(object))
      if (private$validateFileName(object, c("Rdata", "RData", "Rda"))[['code']] == FALSE)
        return(private$validateFileName(object, c("Rdata", "RData", "Rda")))
      return(status[['code']] <- TRUE)
    },

    documentXlsx = function(object) {

      if (private$validateName(object)[['code']] == FALSE)
        return(private$validateName(object))
      if (private$validateFileName(object, c("xlsx", "xls"))[['code']] == FALSE)
        return(private$validateFileName(object, c("xlsx", "xls")))
      return(status[['code']] <- TRUE)

    }
  )
)
