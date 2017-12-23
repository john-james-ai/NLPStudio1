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

      private$..admin$className <- 'File'
      private$..admin$methodName <- 'initialize'
      private$..admin$name <- name
      private$..admin$path <- path
      private$..fileName <- basename(path)
      private$..io <- IOFactory$new()$getIOStrategy(private$..admin$path)
      private$..admin$state <- paste("File object", private$..admin$name, "instantiated.")
      private$..admin$logs <- LogR$new()
      private$..admin$modified <- Sys.time()
      private$..admin$created <- Sys.time()
      private$..admin$accessed <- Sys.time()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            MetaData Methods                             #
    #-------------------------------------------------------------------------#
    getFileName = function() private$..fileName,

    fileInfo = function() file.info(private$..admin$path),

    #-------------------------------------------------------------------------#
    #                            Access Methods                               #
    #-------------------------------------------------------------------------#
    lock = function() {
      private$..admin$methodName <- 'lock'

      private$..admin$locked <- TRUE

      private$..admin$state <- paste0("File object, ", private$..admin$name, ", locked.")
      private$..admin$modified <- Sys.time()
      self$logIt()
      invisible(self)
    },

    unlock = function() {
      private$..admin$methodName <- 'lock'

      private$..admin$locked <- FALSE

      private$..admin$state <- paste0("File object, ", private$..admin$name, ", unlocked.")
      private$..admin$modified <- Sys.time()
      self$logIt()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                               IO Methods                                #
    #-------------------------------------------------------------------------#
    read = function(io = NULL) {

      private$..admin$methodName <- 'read'

      if (is.null(io)) io <-  private$..io

      private$..content <- io$read(private$..admin$path)

      # LogIt
      private$..admin$state <- paste0("Read ", private$..admin$name, ". ")
      private$..admin$accessed <- Sys.time()
      self$logIt()

      return(private$..content)
    },

    write = function(io = NULL) {

      private$..admin$methodName <- 'write'

      if (private$..admin$locked == TRUE) {
        private$..admin$state <- paste0("Unable to write to ", private$..admin$name,
                                  ", the file is locked.")
        self$logIt("Warn")
        stop()
      }

      if (is.null(io)) io <- private$..io

      io$write(private$..admin$path, private$..content)

      # LogIt
      private$..admin$state <- paste0("Wrote ", private$..admin$name, ". ")
      private$..admin$accessed <- Sys.time()
      self$logIt()

      invisible(self)
    },

    flush = function() {
      private$..content <- NULL
    },

    #-------------------------------------------------------------------------#
    #                            Explose Object                               #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      o <- list(
        content = private$..content,
        metaData = list(
          name	 = 	    private$..admin$name ,
          fileName = private$..fileName,
          path	 = 	    private$..admin$path ,
          state	 = 	    private$..admin$state ,
          modified	 = 	private$..admin$modified ,
          created	 = 	  private$..admin$created ,
          accessed	 = 	private$..admin$accessed
        )
      )
      return(o)
    }

  )
)
