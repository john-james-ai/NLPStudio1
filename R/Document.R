#==============================================================================#
#                               Document                                       #
#==============================================================================#
#' Document
#'
#' \code{Document} Class containing the data and methods for corpus documents
#'
#' Defines the object and behavior of the Document object, a composite of
#' the Corpus class.
#'
#' @section Document core methods:
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a document. Not implemented for the abstract class.}
#'   \item{\code{getName()}}{Method that returns the name of the Document object. }
#'   \item{\code{getFileName()}}{Method for obtaining the document file name. .}
#'   \item{\code{getPath()}}{Method for obtaining the document path. }
#'  }
#'
#' @section Document getter/setter methods:
#'  \itemize{
#'   \item{\code{desc()}}{Method for setting or retrieving the Document object description.}
#'  }
#'
#'  @section Document IO methods:
#'  \itemize{
#'   \item{\code{getContent()}}{Method for obtaining the document content.}
#'   \item{\code{addContent(content)}}{Method for adding content to a document.}
#'   \item{\code{read(io)}}{Method for reading a document. }
#'   \item{\code{write(io)}}{Method for writing a document. }
#'  }
#'
#' @section Document aggregation method:
#'  \itemize{
#'   \item{\code{setParent(parent)}}{Sets the parent Corpus object. }
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
#' @family Document classes
#' @export
Document <- R6::R6Class(
  classname = "Document",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..content = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name, path = NULL) {

      # Instantiate variables
      private$..className <- 'Document'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..path <- path
      private$..content <- NULL
      private$..parent <- NULL
      private$..state <- paste("Document", private$..name, "instantiated at", Sys.time())
      private$..logs <- LogR$new()
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..accessed <- Sys.time()

      # Validate Document
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      if (is.null(path)) {
        private$..io <- NULL
      } else {
        private$..io <- IOFactory$new()$getIOStrategy(path = path)
      }

      # Create log entry
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                         Content Methods                                 #
    #-------------------------------------------------------------------------#
    getContent = function() {

      private$..methodName <- 'getContent'

      private$..accessed <- Sys.time()

      if (is.null(private$..content)) {
        if (is.null(private$..path)) {
          content <- NULL
        } else {
          content <- self$read()
          }
        } else {
        content <- private$..content
        }
      return(content)
    },

    addContent = function(content) {

      private$..modified <- Sys.time()
      private$..accessed <- Sys.time()
      private$..content <- content

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            IO Methods                                   #
    #-------------------------------------------------------------------------#
    read = function() {

      private$..methodName <- 'read'

      status <- private$..io$read(path = private$..path)

      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt('Error')
        stop()
      } else {
        private$..content <- status[['data']]
      }

      # LogIt
      private$..state <- paste0("Read ", private$..name, ". ")
      private$..accessed <- Sys.time()
      self$logIt()

      return(private$..content)

    },

    write = function() {

      private$..methodName <- 'write'

      status <- private$..io$write(content = private$..content,
                                   path = private$..path)

      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt('Error')
        stop()
      }

      # LogIt
      private$..state <- paste0("Wrote ", private$..name, ". ")
      private$..accessed <- Sys.time()
      private$..modified <- Sys.time()
      self$logIt()

      return(TRUE)
    },

    #-------------------------------------------------------------------------#
    #                           Aggregatw Methods                             #
    #-------------------------------------------------------------------------#
    setParent = function(parent) {

      private$..methodName <- 'setParent'

      v <- Validator$new(self)
      status <- v$setParent(self, parent)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt('Error')
        stop()
      }

      private$..parent <- parent
      private$..modified <- Sys.time()
      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$document(self)
    },

    #-------------------------------------------------------------------------#
    #                           Expose Object                                 #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      o <- list(
        className	 =  private$..className ,
        name	 = 	    private$..name ,
        desc	 = 	    private$..desc ,
        parent	 = 	  private$..parent ,
        path	 = 	    private$..path ,
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
