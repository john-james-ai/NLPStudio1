#==============================================================================#
#                                   FileRdata                                  #
#==============================================================================#
#' FileRdata
#'
#' \code{FileRdata} Class containing the methods for reading, writing and repairing Rdata files.
#'
#' Class contains methods for reading, writing and repairing Rdata files containing text content.
#'
#' @section FileRdata core methods:
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a file. Not implemented for the abstract class.}
#'   \item{\code{getName()}}{Method that returns the name of the File object. }
#'   \item{\code{getFileName()}}{Method for obtaining the file file name. .}
#'   \item{\code{getPath()}}{Method for obtaining the file path. }
#'  }
#'
#' @section FileRdata getter/setter methods:
#'  \itemize{
#'   \item{\code{desc()}}{Method for setting or retrieving the FileRdata object description.}
#'  }
#'
#'  @section FileRdata IO methods:
#'  \itemize{
#'   \item{\code{read()}}{Method for reading a file. }
#'   \item{\code{write()}}{Method for writing a file. }
#'   \item{\code{repair()}}{Method for repairing a file. }
#'  }
#'
#' @section FileRdata aggregation method:
#'  \itemize{
#'   \item{\code{setParent(parent)}}{Sets the parent file for the File object.}
#'  }
#'
#'
#' @section Other methods:
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Accepts a visitor object. Not implemented for this abstract class}
#'   \item{\code{logIt(level = 'Info')}}{Logs events relating to the file.}
#'  }
#'
#' @param name Character string containing the name of the File object
#' @param path Character string indicating the file path
#' @param parent Document object to which this file is associated
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family File classes
#' @export
FileRdata <- R6::R6Class(
  classname = "FileRdata",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = File0,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name, path) {

      # Instantiate variables
      private$..className <- 'FileRdata'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..path <- path
      private$..content <- NULL
      private$..state <- paste0("FileRdata, ", private$..name, ", instantiated at", Sys.time())
      private$..logs <- LogR$new()
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..accessed <- Sys.time()

      # Validate FileRdata
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
      visitor$fileRdata(self)
    }
  )
)
