#==============================================================================#
#                               DocumentText                                   #
#==============================================================================#
#' DocumentText
#'
#' \code{DocumentText} Class that contains document meta data and text content.
#'
#' This class contains the data and methods for creating, reading, manipulating,
#' processing, and transforming text documents.
#'
#' @section Document Family Participants:
#'  \itemize{
#'   \item{Document0}{This abstract class defines the interface for the other classes.}
#'   \item{DocumentText}{Class for text documents.}
#'   \item{DocumentNGrams}{Class for NGrams.}
#'   \item{DocumentPOSTags}{Class for POS tags.}
#'  }
#'
#' @section Document core methods:
#'  \itemize{
#'   \item{\code{new(filePath, desc = NULL)}}{Method for instantiating a document.}
#'   \item{\code{getName()}}{Method that returns the name of the Document object.}
#'   \item{\code{getFileName()}}{Method for obtaining the document file name.}
#'   \item{\code{getPath()}}{Method for obtaining the document path.}
#'   \item{\code{readDocument(io)}}{Method for initiating the read operation for a document.}
#'   \item{\code{writeDocument(io)}}{Method for initiating the write operation for a document.}#'
#'   \item{\code{repairDocument(repairs)}}{Method for repairing a document.}
#'   \item{\code{normalizeDocument(norms)}}{Method for normalizing text in a document.}
#'   \item{\code{sanitizeDocument(profanity)}}{Method for removing profanity from a text document.}
#'  }
#'
#' @section Document getter/setter methods:
#'  \itemize{
#'   \item{\code{name()}}{Method for setting or retrieving the Document object name.}
#'   \item{\code{desc()}}{Method for setting or retrieving the Document object description.}
#'
#' @param filePath Character string indicating the file path for a document
#' @param parent Object of the Corpus or Set classes to which this document belongs
#' @param io An object of one of the IO classes used for reading and writing.
#' @param repairs A list of key value pairs of strings to be replaced in the repair step
#' @param norms A list of key value pairs of strings to be replaced in the normalization step
#' @param corrections A list of key value pairs of strings to be replaced in the corrections step
#' @param profanity Character vector of profane words to be extracted from the text
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document classes
#' @export
DocumentText <- R6::R6Class(
  classname = "DocumentText",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Document0,

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
      private$..parent <- NULL
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

      # Repair File
      io <- IOBin$new()
      content <- self$read(io)
      content[content == as.raw(0)] = as.raw(0x20)
      content[content == as.raw(26)] = as.raw(0x20)
      private$..content <-  content
      self$write(io)

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
    getContent = function() private$..content,


    #-------------------------------------------------------------------------#
    #                            IO Methods                                   #
    #-------------------------------------------------------------------------#
    read = function(io = NULL) {

      if (is.null(io)) {
        io <- IOText$new()
      }
      status <- io$read(self)

      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      } else {
        private$..content <- status[['data']]
      }
    },

    write = function(io = NULL) {

      if (is.null(io)) {
        io <- IOText$new()
      }
      status <- io$write(self)

      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }
    },

    #-------------------------------------------------------------------------#
    #                            Log Method                                   #
    #-------------------------------------------------------------------------#
    logIt = function(level = 'Info', fieldName = NA) {

      private$..logs$entry$owner <- private$..name
      private$..logs$entry$className <- private$..className
      private$..logs$entry$methodName <- private$..methodName
      private$..logs$entry$level <- level
      private$..logs$entry$msg <- private$..state
      private$..logs$entry$fieldName <- fieldName
      private$..logs$created <- Sys.time()
      private$..logs$writeLog()
    },
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
        parent	 = 	  private$..corpus ,
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
