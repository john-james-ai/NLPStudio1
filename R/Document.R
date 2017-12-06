#==============================================================================#
#                                   Document                                  #
#==============================================================================#
#' Document
#'
#' \code{Document} An abstract class that defines the interfaces for the document sub-classes
#'
#' The Document family of classes is an implementation of the strategy
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows behavior
#' of a class to be defined at runtime.
#'
#' @section Document family of classes participants:
#' The participants of the Document class are:
#' \itemize{
#'  \item Document: This "abstract" class defines the interface for document sub-classes.
#'  \item DocumentText: Document class for text documents.
#'  \item DocumentNGram: Document class for NGrams
#'  \item DocumentPOS: Document class for POS tagged documents.
#'  }
#'
#' @section Document class collaborators:
#' The collaborators of the Document family  are:
#'  \itemize{
#'   \item Korpus: Class containing one or more documents
#'   \item Reader: Class responsible for initiating the document read operation.
#'   \item Writer: Class responsible for initiating the document write operation.
#'   \item FileManager: Class responsible for managing and repairing files
#'   \item VReader: Visitor class responsible for performing read operations through the Document hierarchy.
#'   \item VWriter: Visitor class responsible for performing write operations through the Document hierarchy.
#'   \item NGrams: Class responsible for creating nGram representations of the documents
#'   \item POSTags: Class responsible for creating POS tagged representations of the documents
#'  }
#'
#'  @section Document methods:
#'  \itemize{
#'   \item{\code{new(name, fileName, desc)}}{Method for instantiating a document}
#'   \item{\code{getName()}}{Method for obtaining the document name.}
#'   \item{\code{desc()}}{Method for retrieving and setting the document description.}
#'   \item{\code{korpus()}}{Method for retrieving and setting the korpus for the document.}
#'   \item{\code{readDocument()}}{Method for initiating the read operation for a document.}
#'   \item{\code{writeDocument(content)}}{Method for initiating the write operation for a document.}
#'   \item{\code{repairDocument()}}{Method for initiating the repair operation on a document.}
#'   \item{\code{cleanDocument(reference)}}{Method for cleaning the document.}
#'   \item{\code{nGramDocument(nGrams)}}{Method for creating an nGram representation of the document.}
#'   \item{\code{posTagDocument(nGrams)}}{Method for creating an POS tagged representation of the document.}
#'  }
#'
#' @param name Character string indicating the name of the document or file. Required for all objects.
#' @param desc Character string containing the description of the document.
#' @param korpus Korpus object to which the document belongs
#' @param content Nested list of content to be written to files.
#' @param fileName Character string indicating File object's file name.
#' @param korpus A Korpus object
#' @param reference A list of data frames containing reference information used in the cleaning methods
#' @param nGrams Numeric indicator of the number of nGrams to produce
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document classes
#' @export
Document <- R6::R6Class(
  classname = "Document",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..className = character(),
    ..docType = character(),
    ..name = character(),
    ..fileName = character(),
    ..desc = character(),
    ..korpus = character(),
    ..path = character(),
    ..content = character(),
    ..size = numeric(),
    ..state = character(),
    ..created = character(),
    ..modified = character(),
    ..accessed = character()
  ),

  active = list(

    name = function(value) {
      if (missing(value)) {
        private$..name
      } else {
        private$..name <- value
      }
    },

    desc = function(value) {
      if (missing(value)) {
        private$..desc
      } else {
        private$..desc <- value
      }
    },

    korpus = function(value) {
      if (missing(value)) {
        private$..parent
      } else {
        v <- Validator$new()
        status <- v$setParent(self, value)
        if (status[['code']] == FALSE) {
          private$..state <- status[['msg']]
          self$logIt(level = 'Error')
          stop()
        } else {
          private$..parent <- value
          private$..path <- file.path(value$getPath(),
                                      value$getDirs()$korpora,
                                      private$..docType,
                                      private$..name)
          private$..logs <- LogR$new(file.path(value$getPath(),
                                               value$getDirs()$korpora,
                                               private$..name, 'logs'))
          private$..modified <- Sys.time()
          private$..state <- paste0('Document ', private$..name, 'added to ',
                                    value$getName(), '.')
          self$logIt()

          # Assign its name in the global environment
          assign(private$..name, self, envir = .GlobalEnv)

          invisible(self)

        }
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(filePath, desc = NULL) stop("This is not implemented for this abstract class."),

    #-------------------------------------------------------------------------#
    #                       Path and Filename Methods                         #
    #-------------------------------------------------------------------------#
    getPath = function() private$..path,
    getFileName = function() private$..fileName,
    getDocType = function() private$..docType,

    readDocument = function() {stop("This method is not implemented for the Document class.")},
    writeDocument = function(content) {stop("This method is not implemented for the Document class.")},

    #-------------------------------------------------------------------------#
    #                            Log Method                                   #
    #-------------------------------------------------------------------------#
    logIt = function(level = 'Info', fieldName = NA) {

      private$..logs$entry$owner <- private$..name
      private$..logs$entry$className <- private$..className
      private$..logs$entry$methodName <- match.call()[[1]]
      private$..logs$entry$level <- level
      private$..logs$entry$msg <- private$..state
      private$..logs$entry$fieldName <- fieldName
      private$..logs$created <- Sys.time()
      private$..logs$writeLog()
    },

    exposeObject = function() {

      document = list(
        className = private$..className,
        docType = private$..docType,
        name = private$..name,
        desc = private$..desc,
        parent = private$..parent,
        path = private$..path,
        fileName = private$..fileName,
        content = private$..content,
        size = private$..size,
        state = private$..state,
        created = private$..created,
        modified = private$..modified,
        accessed = private$..accessed
      )
      return(document)
    }
  )
)
