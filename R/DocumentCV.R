#==============================================================================#
#                               DocumentCV                                     #
#==============================================================================#
#' DocumentCV
#'
#' \code{DocumentCV} Concrete class for the cross validation documents
#'
#' Class conitains the documents used for training and cross-validation
#' purposes.
#'
#' @section DocumentCV core methods:
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a document. Not implemented for the abstract class.}
#'   \item{\code{getName()}}{Method that returns the name of the Document object. }
#'   \item{\code{getFileName()}}{Method for obtaining the document file name. .}
#'   \item{\code{getPath()}}{Method for obtaining the document path. }
#'   \item{\code{getContent()}}{Method for obtaining the document content.}
#'  }
#'
#' @section DocumentCV getter/setter methods:
#'  \itemize{
#'   \item{\code{desc()}}{Method for setting or retrieving the DocumentCV object description.}
#'  }
#'
#'  @section DocumentCV IO methods:
#'  \itemize{
#'   \item{\code{addContent(content)}}{Method for adding content to a document.}
#'   \item{\code{read(io)}}{Method for reading a document. }
#'   \item{\code{write(io)}}{Method for writing a document. }
#'  }
#'
#' @section DocumentCV aggregation method:
#'  \itemize{
#'   \item{\code{move(parent)}}{Moves a document to the designated parent corpus. }
#'  }
#'
#'
#' @section Other methods:
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Accepts a visitor object. Not implemented for this abstract class}
#'   \item{\code{logIt(level = 'Info')}}{Logs events relating to the document.}
#'  }
#'
#'
#' @param filePath Character string indicating the file path for a document
#' @param parent Object of the Corpus or Set classes to which this document belongs
#' @param io An object of one of the IO classes used for reading and writing.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family DocumentCV classes
#' @export
DocumentCV <- R6::R6Class(
  classname = "DocumentCV",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = DocumentCV,

  private = list(
    ..io = character(),
    ..content = character(),
    ..size = numeric()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(filePath, io, desc = NULL) {

      private$..methodName <- 'initialize'

      # Instantiate variables
      private$..className <- 'Document'
      private$..name <- tools::file_path_sans_ext(basename(filePath))
      private$..fileName <- basename(filePath)
      private$..desc <- ifelse(is.null(desc), private$..fileName, desc)
      private$..parent <- NULL
      private$..path <- filePath
      private$..io <- io
      private$..content <- NULL
      private$..state <- paste("Document", private$..name, "instantiated at", Sys.time())
      private$..logs <- LogR$new()
      private$..size <- file.info(filePath)$size
      private$..modified <- file.info(filePath)$mtime
      private$..created <- file.info(filePath)$ctime
      private$..accessed <- file.info(filePath)$atime

      # Validate Document
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


    getFileName = function() private$..fileName,
    getIO = function() private$..io,
    getContent = function() {
      if (is.null(private$..content))  self$read()
      private$..content
    },

    #-------------------------------------------------------------------------#
    #                            IO Methods                                   #
    #-------------------------------------------------------------------------#
    cloneContent = function(document) {
      private$..content <- document$getContent()
    },

    read = function(io = NULL) {

      private$..methodName <- 'read'

      if (is.null(io)) io <- private$..io

      status <- io$read(self)

      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      } else {
        private$..content <- status[['data']]
      }

      # LogIt
      private$..state <- paste0("Read ", private$..fileName, ". ")
      private$..accessed <- Sys.time()
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)

    },

    write = function(io = NULL) {

      private$..methodName <- 'write'

      if (is.null(io)) io <- private$..io

      status <- io$write(self)

      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # LogIt
      private$..state <- paste0("Wrote ", private$..fileName, ". ")
      private$..modified <- Sys.time()
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                         Aggregate Methods                               #
    #-------------------------------------------------------------------------#
    move = function(parent) {

      private$..methodName <- 'move'

      v <- Validator$new()
      status <- v$setParent(self, parent)

      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      } else {
        private$..parent <- parent

        # Move File
        from <- private$..path
        to <- file.path(private$..parent$getPath(), 'documents/text', private$..fileName)
        f <- FileManager$new()
        status <- f$moveFile(from, to)
        if (status[['code']] == FALSE) {
          private$..state <- status[['msg']]
          self$logIt(level = 'Error')
          stop()
        }

        private$..path <- to
        private$..modified <- Sys.time()
        private$..state <- paste(private$..className, private$..name, 'moved to ',
                                 parent$getClassName(), parent$getName())
        self$logIt()

        # Assign its name in the global environment
        assign(private$..name, self, envir = .GlobalEnv)

        invisible(self)
      }
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$documentCV(self)
    },

    #-------------------------------------------------------------------------#
    #                           Expose Object                                 #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      o <- list(
        className	 =  private$..className ,
        name	 = 	    private$..name ,
        fileName	 =  private$..fileName ,
        desc	 = 	    private$..desc ,
        parent	 = 	  private$..parent ,
        path	 = 	    private$..path ,
        io = private$..io,
        content =     private$..content,
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
