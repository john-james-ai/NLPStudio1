#==============================================================================#
#                                  File                                        #
#==============================================================================#
#' File
#'
#' \code{File} Class representing a file in the NLPSTudio package.
#'
#' Class contains the methods and data for a file.  Methods include reading
#' writing, locking, unlocking, and repairing a File.
#'
#' @section File Methods:
#'  \describe{
#'   \item{\code{new()}}{Creates an object of File Class}
#'   \item{\code{lock()}}{Makes a File read-only. }
#'   \item{\code{unlock()}}{Opens write access on a File. }
#'   \item{\code{read(io)}}{Reads a File.}
#'   \item{\code{write(io, content)}}{Writes a File.}
#'  }
#'
#' @section Parameters:
#' @param io IO Object indicating the mode for reading and writing the File.
#' @param locked Boolean indicating whether a File is locked.
#' @param name A character string containing the name of the File object.
#' @param path Character string indicating the relative path, including the file name,  for the File object
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
File <- R6::R6Class(
  classname = "File",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = FileCollection,

  private = list(
    ..content = character()
  ),

  active = list(

    content = function(value) {
      if (missing(value)) {
        return(private$..content)
      } else {
        private$..content <- value
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           File Core Methods                             #
    #-------------------------------------------------------------------------#
    initialize = function(name, path) {

      private$..className <- 'File'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..path <- path
      private$..fileName <- basename(path)
      private$..io <- IOFactory$new()$getIOStrategy(private$..path)
      private$..state <- paste("File object", private$..name, "instantiated.")
      private$..logs <- LogR$new()
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..accessed <- Sys.time

      if (file.exists(private$..path)) self$read()

      invisible(self)
    },

    getFileName = function() private$..fileName,

    #-------------------------------------------------------------------------#
    #                            Access Methods                               #
    #-------------------------------------------------------------------------#
    lock = function() {
      private$..methodName <- 'lock'

      private$..locked <- TRUE

      private$..state <- paste0("File object, ", private$..name, ", locked.")
      private$..modified <- Sys.time()
      self$logIt()
      invisible(self)
    },

    unlock = function() {
      private$..methodName <- 'lock'

      private$..locked <- FALSE

      private$..state <- paste0("File object, ", private$..name, ", unlocked.")
      private$..modified <- Sys.time()
      self$logIt()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                               IO Methods                                #
    #-------------------------------------------------------------------------#
    read = function(io = NULL) {

      private$..methodName <- 'read'

      if (is.null(io)) io <-  private$..io

      private$..content <- io$read(private$..path)

      # LogIt
      private$..state <- paste0("Read ", private$..name, ". ")
      private$..accessed <- Sys.time()
      self$logIt()

      return(private$..content)
    },

    write = function(io = NULL) {

      private$..methodName <- 'write'

      if (private$..locked == TRUE) {
        private$..state <- paste0("Unable to write to ", private$..name,
                                  ", the file is locked.")
        self$logIt("Warn")
        stop()
      }

      if (is.null(io)) io <- private$..io

      io$write(private$..path, private$..content)

      # LogIt
      private$..state <- paste0("Wrote ", private$..name, ". ")
      private$..accessed <- Sys.time()
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            Explose Object                               #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      o <- list(
        content = private$..content,
        metaData = list(
          name	 = 	    private$..name ,
          fileName = private$..fileName,
          path	 = 	    private$..path ,
          state	 = 	    private$..state ,
          modified	 = 	private$..modified ,
          created	 = 	  private$..created ,
          accessed	 = 	private$..accessed
        )
      )
      return(o)
    }

  )
)
