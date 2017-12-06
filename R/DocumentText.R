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
DocumentText <- R6::R6Class(
  classname = "DocumentText",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Document,

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

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(filePath, desc = NULL) {

      # Instantiate variables
      private$..className <- 'DocumentText'
      private$..docType <- 'Text'
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
    getName = function() private$..name,
    getFileName = function() private$..fileName,
    getPath = function() private$..path,

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
