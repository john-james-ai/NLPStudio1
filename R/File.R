#==============================================================================#
#                                  File                                        #
#==============================================================================#
#' File
#'
#' \code{File} Class containing the file object and its methods.
#'
#' Class contains the file object and all methods required for reading, writing,
#' downloading, compressing, and repairing files.
#'
#' @section File Methods:
#'  \describe{
#'   \item{\code{new()}}{Creates an object of File Class}
#'   \item{\code{move(from, to)}}{Move a file or directory. }
#'   \item{\code{copy(from, to)}}{Copy a file or directory. }
#'   \item{\code{zip(zipFile, files)}}{Compresses files or files and stores them in the filepath indicated by zipFile.}
#'   \item{\code{unzip(zipFile, to)}}{Extract compressed files and places them in the directory indicated by exDir.}
#'   \item{\code{download(url, directory)}}{Downloads a file and stores it in the file designated by the fileName  .}
#'   \item{\code{lock()}}{Makes a file read-only. }
#'   \item{\code{unlock()}}{Opens write access on a file. }
#'   \item{\code{read(io)}}{Reads a file.}
#'   \item{\code{write(io, content)}}{Writes a file.}
#'   \item{\code{repair()}}{Repairs file encoding.}
#'  }
#'
#' @section Parameters:
#' @param directory Character string indicating a directory.
#' @param fileName Character string containing the file name.
#' @param io IO Object indicating the mode for reading and writing the file.
#' @param locked Boolean indicating whether a file is locked.
#' @param name A character string containing the name of the File object. This variable is used in the instantiation and remove methods.
#' @param path Character string containing the relative path, including the file name, to a file.
#' @param to Character string indicating the directory of file path to which a file is copied or moved.
#' @param url Character string containing the URL from which a file will be downloaded.
#' @param zipFiles Cheracter vector containing the names of files to be extracted.
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
    ..fileName = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         File Core Methods                               #
    #-------------------------------------------------------------------------#
    initialize = function(name, path) {

      private$..className <- 'File'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..directory <- dirname(path)
      private$..path <- path
      private$..fileName <- basename(path)
      private$..io <- IOFactory$new()$getIOStrategy(path = path)
      private$..state <- paste("File object", private$..name, "instantiated.")
      private$..logs <- LogR$new()
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..accessed <- Sys.time()

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

      if (is.null(io)) {
        content <- private$..io$read(self)
      } else {
        content <- io$read(self)
      }

      # LogIt
      private$..state <- paste0("Read ", private$..name, ". ")
      private$..accessed <- Sys.time()
      self$logIt()

      return(content)

    },

    write = function(content, io = NULL) {

      private$..methodName <- 'write'

      if (private$..locked == TRUE) {
        private$..state <- paste0("Unable to write ", private$..fileName, ". ",
                                  "The file is locked. See ?", private$..className,
                                  " for further assistance.")
        self$logIt('Error')
        stop()
      }

      if (is.null(io)) {
        private$..io$write(self, content)
      } else {
        io$write(self, content)
      }

      # LogIt
      private$..state <- paste0("Wrote ", private$..name, ". ")
      private$..accessed <- Sys.time()
      self$logIt()

      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                         Move/Copy/Remove Methods                        #
    #-------------------------------------------------------------------------#
    moveFile = function(to)  {

      private$..methodName <- 'moveFile'

      if (private$..locked == TRUE) {
        private$..state <- paste0('File is locked. It cannot be moved. See ?',
                                  private$..className, " for further assistance.")
        self$logIt('Error')
        stop()
      }

      if (missing(to)) {
        private$..state <- paste0('Please indicate the directory into which
                                 the file should be moved. See ?',
                                  private$..className, " for further assistance.")
        self$logIt('Error')
        stop()
      }

      todir <- dirname(to)
      dir.create(todir, showWarnings = FALSE, recursive=TRUE)
      file.rename(from = private$..path,  to = to)
      private$..path <- to
      private$..directory <- dirname(to)

      private$..state <-
        paste0("Successfully moved file(s) to ", to, "." )
      self$logIt()

      invisible(self)
    },

    removeFile = function() {

      private$..methodName <- 'removeFile'

      file.remove(private$..path)
      private$..state <- paste0("Successfully removed ", basename(from), ".")
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            Expose Object                                #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      o <- list(
        className	 =  private$..className ,
        name	 = 	    private$..name ,
        directory = private$..directory,
        path	 = 	    private$..path ,
        fileName = private$..fileName,
        io = private$..io,
        state	 = 	    private$..state ,
        logs	 = 	    private$..logs ,
        modified	 = 	private$..modified ,
        created	 = 	  private$..created ,
        accessed	 = 	private$..accessed
      )
      return(o)
    }
  )
)
