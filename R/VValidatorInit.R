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
#'   \item{\code{studio(object)}}{Method for validating the instantiation of the Studio object}
#'   \item{\code{documentCollection(object)}}{Method for validating the instantiation of the DocumentCollection object.}
#'   \item{\code{document(object)}}{Method for validating the instantiation of the Document object.}
#'   \item{\code{documentCSV(object)}}{Method for validating the instantiation of the DocumentCSV object.}
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
    ..object = character(),
    ..parent = character(),

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

    validateParent = function(object, parentClass) {

      status <- list()
      status[['code']] <- TRUE

      name <- object$getName()
      parent <- private$..parent

      # Confirm parent is not missing
      if (is.null(parent)) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Cannot create ", class(object)[1],
                                  " object, ", name, ". Parent parameter is ",
                                  "missing with no default. ",
                                  "See ?", class(object)[1],
                                  " for further assistance")
        return(status)
      }

      # Confirm parent is valid class
      v <- ValidatorClass$new()
      if (v$validate(value = parent, expect = parentClass) == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Cannot create ", class(object)[1],
                                  " object, ", name, ". Object of class ",
                                  class(object)[1], " can not have an ",
                                  "object of class ", class(parent)[1],
                                  " as a parent. ",
                                  "See ?", class(object)[1],
                                  " for further assistance")
        return(status)
      }
      return(status)
    },

    validateUrl = function(object) {

      status <- list()
      status[['code']] <- TRUE

      url <- object$getURL()

      # Confirm URL is not missing
      if (is.null(url)) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Cannot create ", class(object)[1],
                                  " object, ", name, ". URL is missing",
                                  "with no default. ",
                                  "See ?", class(object)[1],
                                  " for further assistance")
        return(status)
      }

      # Confirm URL is valid class
      v <- ValidatorUrl$new()
      if (v$validate(value = url, expect = NULL) == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Cannot create ", class(object)[1],
                                  " object, ", name, ". URL is invalid. ",
                                  "See ?", class(object)[1],
                                  " for further assistance")
        return(status)
      }
      return(status)
    },

    validateArchive = function(object) {
      status <- list()
      status[['code']] <- TRUE

      name <- object$getName()
      archiveClasses <- c('Studio', 'Pipeline', 'Feature', 'Model',  'Data',
                          'Corpus', 'Document')

      v <- ValidatorClass$new()
      if (v$validate(value = object, expect = archiveClasses) == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Cannot create archive for ", class(object)[1],
                                  " object, ", name, ". Objects of class ",
                                  class(object)[1], " are not archivable. ",
                                  "See ?", class(object)[1],
                                  " for further assistance")
        return(status)
      }
      return(status)
    },

    validateFileType = function(object) {

      status <- list()
      status[['code']] <- TRUE

      path <- object$getPath()
      type <- tolower(tools::file_ext(path))

      if (!(type %in% c('txt', 'csv', 'rdata', 'rds'))) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Cannot create File Strategy for ", class(object)[1],
                                  " object, ", name, ". File type, ", type,
                                  " is not supported. ",
                                  "See ?", class(object)[1],
                                  " for further assistance")
        return(status)
      }
      return(status)
    }
  ),

  public = list(

    initialize = function(object) {
      invisible(self)
    },

    nlpStudio = function(object) {
      return(status[['code']] <- TRUE)
    },

    pipeline = function(object) {
      return(private$validateName(object))
    },

    corpus = function(object) {
      return(private$validateName(object))
    },

    document = function(object) {
      return(private$validateName(object))
    }
  )
)
