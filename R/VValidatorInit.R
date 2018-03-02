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
    
    validateTokenize = function(object) {
      status <- list()
      status[['code']] <- TRUE
      
      params <- object$getParams()
      
      
      if (!("Corpus" %in% class(params$x))) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Invalid to object. Object must be of the Corpus class. ",
                                  "See ?", class(object)[1], " for further assistance.")
      } else {
        if (!(params$what %in% c("word", "sentence"))) {
          status[['code']] <- FALSE
          status[['msg']] <- paste0("Invalid to parameter value. Valid values are ",
                                    "c('word', 'sentence'). ",
                                    "See ?", class(object)[1], " for further assistance.")
        }
      }
      return(status)
    },
    
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

    validateDirGlob = function(object) {

      status <- list()
      status[['code']] <- TRUE
      
      files <- character(0)
      
      ds <- object$getParams()

      if (isDirectory(ds)) {
        files <- list.files(ds, full.names = TRUE)
      } else if ("character" %in% class(ds)) {
        glob <- basename(ds)
        dir <- dirname(ds)
        files <- list.files(dir, pattern = glob2rx(glob), full.names = TRUE)
      } 
      
      if (length(files) == 0) {
        
        status$code <- FALSE
        status$msg <- paste0("Unable to import corpus from ",
                             ds, ". ",
                             "No files match the data source. See ?",
                             class(object)[1], " for further assistance.")
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
    
    validateAdaptor = function(object, formats, classes) {
      
      status <- list()
      status[['code']] <- TRUE
      
      params <- object$getParams()
      x <- params$x
      format <- params$format
      if (sum(grepl(class(x)[1], classes)) == 0) {
        status$code <- FALSE
        status$msg <- paste0("Invalid object class for adaptation.  Parameter x must ",
                            "be a valid corpus object from the NLPStudio, Quanteda, ",
                            "TM, textclean, koRpus, or qdap packages.")
      } else if (sum(grepl(format, formats)) == 0) {
        status$code <- FALSE
        status$msg <- paste0("Invalid object format for adaptation.  Format parameter must ",
                            "be in ", paste(formats, collapse = ""), ". ")
      }
      
      return(status)
    },
    
    
    validateMeta = function(object) {
      
      status <- list()
      status[['code']] <- TRUE
      
      name <- object$meta(key = "name")
      
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
    
    validateStub = function(object) {
      status <- list()
      status[['code']] <- TRUE
      return(status)
    }
  ),

  public = list(
    
    #-------------------------------------------------------------------------#
    #                        Validate Core Classes                            #
    #-------------------------------------------------------------------------#
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
    #                          Metadata Class(es)                             #
    #-------------------------------------------------------------------------#
    meta = function(object) {
      return(private$validateMeta(object))
    },
    
    #-------------------------------------------------------------------------#
    #                           Adaptor Validation                            #
    #-------------------------------------------------------------------------#
    adaptorQ = function(object) {
      formats <- c("q", "Q", "quanteda", "Quanteda",  "QUANTEDA")
      classes <- c("Corpus", "corpus")
      return(private$validateAdaptor(object, formats, classes))
    },
    
    adaptorTM = function(object) {
      formats <- c("tm", "TM")
      classes <- c("Corpus", "VCorpus", "PCorpus", "SimpleCorpus", "DCorpus")
      return(private$validateAdaptor(object, formats, classes))
    },

    #-------------------------------------------------------------------------#
    #                      Corpus Import Validation                           #
    #-------------------------------------------------------------------------#
    sourceDir = function(object) {
      return(private$validateDirGlob(object))
    },
    sourceQuanteda = function(object) {
      return(private$validateClass(object, object$getParams(),
                                   "corpus"))
    },
    sourceVector = function(object) {
      return(private$validateClass(object, object$getParams(),
                                   c("character", "list")))
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
    },
    
    splitCorpus = function(object) {
      return(private$validateClass(object, object$getParams()$x, c("Corpus")))
    },
    
    splitDocument = function(object) {
      return(private$validateClass(object, object$getParams()$x, c("Document")))
    },
    
    tokenize = function(object) {
      return(private$validateTokenize(object))
    }
  )
)
