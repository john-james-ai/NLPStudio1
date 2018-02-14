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
    
    validateLogical = function(object, param, name) {
      status <- list()
      status[['code']] <- TRUE
      
      if (!(is.logical(param))) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Invalid logical, ", name, " must be TRUE or FALSE. ",
                                  "See ?", class(object)[1], " for further assistance.")
      }
      return(status)
    },

    validateDirGlob = function(path) {

      status <- list()
      status[['code']] <- TRUE

      if (!isDirectory(path) & !isDirectory(dirname(path))) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Invalid path. ", path, " does not exist. ")
      }
      return(status)
    },

    validateClass = function(object, param, cls) {

      status <- list()
      status[['code']] <- TRUE

      if (!(class(param)[1] %in% cls)) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Invalid object class. Object is of the ", class(param)[1],
                                  " class, but must be of the ", cls, " classes. ",
                                  "See ?", class(object)[1], " for further assistance.")
      }

      if (class(param)[1] == "list") {
        element <- param[[1]]
        if (!("character" %in% class(element))) {
          status[['code']] <- FALSE
          status[['msg']] <- paste0("Invalid list class. List must contain character vectors. ",
                                    "See ?", class(object)[1], " for further assistance.")
        }
      }
      return(status)
    },

    validateName = function(object) {

      status <- list()
      status[['code']] <- TRUE

      name <- object$getName()

      # Validate name is well-formed
      v <- ValidatorString$new()
      if (v$validate(value = name, expect = NULL) == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Cannot create ", class(object)[1],
                                  " object. ", "Object name, must be a character ",
                                  "string with no spaces.  See ?", class(object)[1],
                                  " for further assistance")
        return(status)
      }

      # Confirm not missing
      if (is.null(name) | is.na(name)) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Name parameter is missing with no default. ",
                                  "See ?", class(object)[1], " for further assistance.")
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
      v = ValidatorClass$new()
      if (v$validate(value = parent, expect = parentClass) == FALSE) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Cannot create ", class(object)[1],
                                  " object. Object of class ",
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

      url <- object$getParams()[[1]]

      # Confirm URL is not missing
      if (is.null(url)) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Cannot create ", class(object)[1],
                                  " object. URL is missing",
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
                                  " object. URL is invalid. ",
                                  "See ?", class(object)[1],
                                  " for further assistance")
        return(status)
      }
      return(status)
    },
    
    validatePatternReplacement = function(object) {
      
      status <- list()
      status[['code']] <- TRUE
      
      params <- object$getParams()
      pattern <- params$pattern
      replacement <- params$replacement
      
      if ("data.frame" %in% class(pattern)) {
        if (ncol(pattern) == 2)  return(status)
      }
    
      
      # Validate classes
      if ((!(class(pattern)[1] %in% c("data.frame", "character"))) |
          (!(class(replacement)[1] %in% c("data.frame", "character")))) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Cannot process ", class(object)[1],
                                  ". Pattern and replacement variables ",
                                  "must be data.frames or character ",
                                  "vectors. ",
                                  "See ?", class(object)[1],
                                  " for further assistance")    
        return(status)
      }
      
      if ("data.frame" %in% class(pattern))  pattern <- as.character(pattern[,1])
      if ("data.frame" %in% class(replacement))  replacement <- as.character(replacement[,1])
      
      if ((length(pattern) != length(replacement)) & (length(replacement) != 1)) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Cannot process ", class(object)[1],
                                  ". The replacement variable must be of ",
                                  "length = 1 or a length equal to that of ",
                                  "the pattern variable. ",
                                  "See ?", class(object)[1],
                                  " for further assistance") 
        return(status)
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
    nlpStudio = function(object) {
      return(status[['code']] <- TRUE)
    },
    corpus = function(object) {
      return(private$validateName(object))
    },
    document = function(object) {
      return(private$validateName(object))
    },

    #-------------------------------------------------------------------------#
    #                      Corpus Import Validation                           #
    #-------------------------------------------------------------------------#
    corpusImportDir = function(object) {
      return(private$validateClass(object, object$getDataSource(), "character"))
    },
    corpusImportQuanteda = function(object) {
      return(private$validateClass(object, object$getDataSource(),
                                   "corpus"))
    },
    corpusImportText = function(object) {
      return(private$validateClass(object, object$getDataSource(),
                                   c("character", "list")))
    },

    #-------------------------------------------------------------------------#
    #                      Preprocessing Validation                           #
    #-------------------------------------------------------------------------#
    preprocessCorpusBinStrategy = function(object) {
      return(private$validateClass(object, object$getInput(), c("Corpus")))
    },
    preprocessCorpusEncodeStrategy = function(object) {
      return(private$validateClass(object, object$getInput(), c("Corpus")))
    },
    preprocessCorpusReshapeStrategy = function(object) {
      return(private$validateClass(object, object$getInput(), c("Corpus")))
    },
    preprocessCorpusSplitStrategy = function(object) {
      return(private$validateClass(object, object$getInput(), c("Corpus")))
    },
    preprocessDocumentBinStrategy = function(object) {
      return(private$validateClass(object, object$getInput(), c("Document")))
    },
    preprocessDocumentEncodeStrategy = function(object) {
      return(private$validateClass(object, object$getInput(), c("Document")))
    },
    preprocessDocumentReshapeStrategy = function(object) {
      return(private$validateClass(object, object$getInput(), c("Document")))
    },
    preprocessDocumentSplitStrategy = function(object) {
      return(private$validateClass(object, object$getInput(), c("Document")))
    },
    
    #-------------------------------------------------------------------------#
    #                      TextStudio Validation                              #
    #-------------------------------------------------------------------------#
    textSalon = function(object) {
      return(private$validateClass(object, object$getInput(),
                                   c("Corpus", "Document", "character", "list")))
    },
    
    replaceAbbreviations = function(object) {
      classes <- c("Corpus", "Document", "list", "character")
      params <- object$getParams()
      status <- private$validateClass(object, params$x, classes)
      if (status$code == FALSE) return(status)
      if (!is.null(params$ignoreCase)) {
        status <- private$validateLogical(object, params$ignoreCase, "ignoreCase")
        if (status$code == FALSE) return(status)
      }
      return(private$validatePatternReplacement(object))
    },
    
    replaceContractions = function(object) {
      classes <- c("Corpus", "Document", "list", "character")
      params <- object$getParams()
      status <- private$validateClass(object, params$x, classes)
      if (status$code == FALSE) return(status)
      if (!is.null(params$leadspace)) {
        status <- private$validateLogical(object, params$leadspace, "leadspace")
        if (status$code == FALSE) return(status)
      }
      if (!is.null(params$trailspace)) {
        status <- private$validateLogical(object, params$trailspace, "trailspace")
        if (status$code == FALSE) return(status)
      }
      if (!is.null(params$fixed)) {
        status <- private$validateLogical(object, params$fixed, "fixed")
        if (status$code == FALSE) return(status)
      }
      if (!is.null(params$trim)) {
        status <- private$validateLogical(object, params$trim, "trim")
        if (status$code == FALSE) return(status)
      }
      return(private$validatePatternReplacement(object))
    },
    
    replaceInternetSlang = function(object) {
      classes <- c("Corpus", "Document", "list", "character")
      params <- object$getParams()
      status <- private$validateClass(object, params$x, classes)
      if (status$code == FALSE) return(status)
      if (!is.null(params$ignoreCase)) {
        status <- private$validateLogical(object, params$ignoreCase, "ignoreCase")
        if (status$code == FALSE) return(status)
      }
      return(private$validatePatternReplacement(object))
    },
    
    replaceTokens = function(object) {
      classes <- c("Corpus", "Document", "list", "character")
      params <- object$getParams()
      status <- private$validateClass(object, params$x, classes)
      if (status$code == FALSE) return(status)
      if (!is.null(params$leadspace)) {
        status <- private$validateLogical(object, params$leadspace, "leadspace")
        if (status$code == FALSE) return(status)
      }
      if (!is.null(params$trailspace)) {
        status <- private$validateLogical(object, params$trailspace, "trailspace")
        if (status$code == FALSE) return(status)
      }
      if (!is.null(params$fixed)) {
        status <- private$validateLogical(object, params$fixed, "fixed")
        if (status$code == FALSE) return(status)
      }
      if (!is.null(params$trim)) {
        status <- private$validateLogical(object, params$trim, "trim")
        if (status$code == FALSE) return(status)
      }
      return(private$validatePatternReplacement(object))
    }
  )
)
