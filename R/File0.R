#==============================================================================#
#                                   File0                                      #
#==============================================================================#
#' File0
#'
#' \code{File0} Abstract class for the File Strategy Classes
#'
#' Defines the base methods for instantiating, reading and writing files.
#'
#' @section File0 core methods:
#'  \itemize{
#'   \item{\code{new()}}{Method for instantiating a file. Not implemented for the abstract class.}
#'   \item{\code{getName()}}{Method that returns the name of the File object. }
#'   \item{\code{getFileName()}}{Method for obtaining the file file name. .}
#'   \item{\code{getPath()}}{Method for obtaining the file path. }
#'  }
#'
#' @section File0 getter/setter methods:
#'  \itemize{
#'   \item{\code{desc()}}{Method for setting or retrieving the File0 object description.}
#'  }
#'
#'  @section File0 IO methods:
#'  \itemize{
#'   \item{\code{read()}}{Method for reading a file. }
#'   \item{\code{write()}}{Method for writing a file. }
#'   \item{\code{repair()}}{Method for repairing a file. }
#'  }
#'
#' @section File0 aggregation method:
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
File0 <- R6::R6Class(
  classname = "File0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Document0,

  private = list(
    ..content = character(),
    ..size = numeric()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name, path) { stop("This method is not implemented for this strategy abstract class.") },

    #-------------------------------------------------------------------------#
    #                            IO Methods                                   #
    #-------------------------------------------------------------------------#
    read = function(io = NULL) {

      private$..methodName <- 'read'

      if (is.null(io))  io <- VIO$new()

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

    write = function() {

      private$..methodName <- 'write'

      if (is.null(io))  io <- VIO$new()

      status <- io$write(self)

      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # LogIt
      private$..state <- paste0("Wrote ", private$..fileName, ". ")
      private$..accessed <- Sys.time()
      private$..modified <- Sys.time()
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       File Proessing Method                             #
    #-------------------------------------------------------------------------#
    repair = function() {

      # Read data in binary format
      io <- IOBin$new()
      self$read(io)

      # Repair content
      content <- private$..content
      content[content == as.raw(0)] = as.raw(0x20)
      content[content == as.raw(26)] = as.raw(0x20)
      private$..content <- content

      # Write
      self$write(io)

      # Read in orignal format
      self$write()

      # LogIt
      private$..state <- paste0("Repaired ", private$..fileName, ". ")
      private$..accessed <- Sys.time()
      private$..modified <- Sys.time()
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                          Aggregate Method                               #
    #-------------------------------------------------------------------------#
    setParent = function(parent) {

      private$..methodName <- 'setParent'

      v <- Validator$new()
      status <- v$setParent(self, parent)

      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      } else {

        private$..parent <- parent

        # Log it
        private$..accessed <- Sys.time()
        private$..modified <- Sys.time()
        private$..state <- paste(private$..className, private$..name, 'parent set to ',
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
