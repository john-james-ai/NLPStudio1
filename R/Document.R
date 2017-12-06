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
#'   \item{\code{preDocument(rapairs)}}{Method for repairing and preprocessing a document.}
#'   \item{\code{normalize(norms)}}{Method for normalizing text in a document.}
#'   \item{\code{correct(corrections)}}{Method for correcting common misspellings, contractoins, etc...in the text of a document.}
#'   \item{\code{profanity(badWords)}}{Method for removing profanity from a text document.}
#'   \item{\code{nGram(nGrams)}}{Method for creating nGrams for a document.}
#'   \item{\code{posTagDocument(nGrams)}}{Method for creating an POS tagged representation of the document.}
#'  }
#'
#' @param filePath Character string indicating the file path for a document
#' @param Korpus The Korpus object to which the document belongs
#' @param repairs A list of key value pairs of strings to be replaced in the repair step
#' @param norms A list of key value pairs of strings to be replaced in the normalization step
#' @param corrections A list of key value pairs of strings to be replaced in the corrections step
#' @param badWords Character vector of profane words to be extracted from the text
#' @param nGrams Numeric indicator of the number of nGrams to produce
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document classes
#' @export
DocumentText <- R6::R6Class(
  classname = "DocumentText",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..className = character(),
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

  #-------------------------------------------------------------------------#
  #                           Active Bindings                               #
  #-------------------------------------------------------------------------#
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
    initialize = function(filePath, desc = NULL) {

      # Instantiate variables
      private$..className <- 'DocumentText'
      private$..name <- basename(filePath)
      private$..fileName <- basename(filePath)
      private$..desc <- ifelse(is.null(desc), private$..fileName, desc)
      private$..korpus <- NULL
      private$..path <- file.path(dirname(filePath), private$..name)
      private$..state <- paste("Document", private$..name, "instantiated at", Sys.time())
      private$..logs <- LogR$new(file.path(NLPStudio$new()$getInstance()$getDirs()$logs))
      private$..size <- file.info(filePath)$size
      private$..modified <- file.info(filePath)$mtime
      private$..created <- file.info(filePath)$ctime
      private$..accessed <- file.info(filePath)$atime

      # Initiate Log
      private$..logs <- LogR$new(file.path(NLPStudio$new()$getInstance()$getDirs()$logs))

      # Validate Lab
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Create log entry
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)

    },
    #-------------------------------------------------------------------------#
    #                           Basic Get Methods                             #
    #-------------------------------------------------------------------------#
    getName = function() private$..name,
    getFileName = function() private$..fileName,
    getPath = function() private$..path,

    #-------------------------------------------------------------------------#
    #                            IO Methods                                   #
    #-------------------------------------------------------------------------#
    readDocument = function() {

      r <- VReader$new()

    },
    writeDocument = function(content) {},
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$documentText(self)
    },

    #-------------------------------------------------------------------------#
    #                           Expose Object                                 #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      o <- list(
        className	 =  private$..className ,
        docType	 = 	  private$..docType ,
        name	 = 	    private$..name ,
        fileName	 =  private$..fileName ,
        desc	 = 	    private$..desc ,
        korpus	 = 	  private$..korpus ,
        path	 = 	    private$..path ,
        state	 = 	    private$..state ,
        logs	 = 	    private$..logs ,
        size	 = 	    private$..size ,
        modified	 = 	private$..modified ,
        created	 = 	  private$..created ,
        accessed	 = 	private$..accessed
      )
      return(o)
    }

  )
)
