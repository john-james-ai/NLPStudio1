#==============================================================================#
#                               DocumentTxt                                    #
#==============================================================================#
#' DocumentTxt
#'
#' \code{DocumentTxt} Class that contains document meta data and text content for a txt file
#'
#' This class contains the data and methods for creating, reading, manipulating,
#' and processing text documents.
#'
#' @section DocumentTxt core methods:
#'  \itemize{
#'   \item{\code{new(filePath, desc = NULL)}}{Method for instantiating a document.}
#'   \item{\code{getName()}}{Method that returns the name of the DocumentTxt object.}
#'   \item{\code{getFileName()}}{Method for obtaining the document file name.}
#'   \item{\code{getPath()}}{Method for obtaining the document path.}
#'   \item{\code{getContent()}}{Method for obtaining the document content.}
#'  }
#'
#' @section DocumentTxt getter/setter methods:
#'  \itemize{
#'   \item{\code{desc()}}{Method for setting or retrieving the DocumentTxt object description.}
#'  }
#'
#'  @section DocumentTxt IO methods:
#'  \itemize{
#'   \item{\code{getContent()}}{Method for obtaining content from a document.}
#'   \item{\code{addContent(content)}}{Method for adding content to a document.}
#'   \item{\code{read(io)}}{Method for reading a document.}
#'   \item{\code{write(io)}}{Method for writing a document.}
#'  }
#'
#' @section DocumentTxt aggregation method:
#'  \itemize{
#'   \item{\code{move(corpus)}}{Moves a document to the designated corpus.}
#'  }
#'
#' @section Other methods:
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Accepts a visitor object.}
#'   \item{\code{logIt(level = 'Info')}}{Logs events relating to the document.}
#'  }
#'
#' @param corpus Parent Corpus object
#' @param name Character string containing the name of the document
#' @param desc Character string containing the description of the document
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family DocumentTxt classes
#' @export
DocumentTxt <- R6::R6Class(
  classname = "DocumentTxt",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = DocumentTxt0,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL) {

      # Instantiate variables
      private$..className <- 'DocumentTxt'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), name, desc)
      private$..path <- file.path(private$..parent$getPath(), private$..fileName)
      private$..io <- IOText$new()
      private$..content <- NULL
      private$..state <- paste("DocumentTxt", private$..name, "instantiated at", Sys.time())
      private$..logs <- LogR$new()
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..accessed <- Sys.time()

      # Validate DocumentTxt
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
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$document(self)
    }
  )
)
