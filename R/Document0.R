#==============================================================================#
#                               Document0                                      #
#==============================================================================#
#' Document0
#'
#' \code{Document0} Abstract clasS for the Document Strategy Classes
#'
#' Defines the base methods for the two document sub-classes, the corpus
#' document and the cross validation set document.
#'
#' @section Document0 core methods:
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a document. Not implemented for the abstract class.}
#'   \item{\code{getName()}}{Method that returns the name of the Document object. }
#'   \item{\code{getFileName()}}{Method for obtaining the document file name. .}
#'   \item{\code{getPath()}}{Method for obtaining the document path. }
#'   \item{\code{getContent()}}{Method for obtaining the document content.}
#'  }
#'
#' @section Document0 getter/setter methods:
#'  \itemize{
#'   \item{\code{desc()}}{Method for setting or retrieving the Document0 object description.}
#'  }
#'
#'  @section Document0 IO methods:
#'  \itemize{
#'   \item{\code{addContent(content)}}{Method for adding content to a document.}
#'   \item{\code{read(io)}}{Method for reading a document. }
#'   \item{\code{write(io)}}{Method for writing a document. }
#'  }
#'
#' @section Document0 aggregation method:
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
#' @family Document0 classes
#' @export
Document0 <- R6::R6Class(
  classname = "Document0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..io = character(),
    ..content = character(),
    ..size = numeric()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function() { stop("This method is not implemented for this strategy abstract class.") },
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
    accept = function(visitor) stop("This method is not implemented for this abstract class."),

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
