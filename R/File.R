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
#' @param from Character string indicating the directory of file path from which a file is copied or moved.
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
  inherit = Entity,

  private = list(
    ..io = character(),
    ..locked = FALSE,
    ..path = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         File Core Methods                               #
    #-------------------------------------------------------------------------#
    initialize = function(name, path) {

      private$..className <- 'File'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..path <- path
      private$..fileName <- basename(path)
      private$..io <- IOFactory$new()$getIOStrategy(path = path)
      private$..parent <- NULL
      private$..state <- paste("File object", private$..name, "instantiated.")
      private$..logs <- LogR$new()
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..accessed <- Sys.time()

      invisible(self)
    },

    getPath = function() private$..path,

    #-------------------------------------------------------------------------#
    #                          Download Method                                #
    #-------------------------------------------------------------------------#
    download = function(url, directory) {

      private$..methodName <- 'download'

      # Format download directory
      fileName <- installr::file.name.from.url(url)

      if (download.file(url, destfile = file.directory(directory, fileName), mode = 'wb') != 0) {
        private$..state <- paste0("Unable to download ", fileName, ".")
        self$logIt('Error')
        stop()
      }
      private$..state <- paste0("Successfully downloaded ", fileName, ". ")
      private$..created <- Sys.time()
      private$..modified <- Sys.time()
      private$..accessed <- Sys.time()
      self$logIt()

      invisible(self)
    },

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

    #-------------------------------------------------------------------------#
    #                          Zip/Unzip Methods                              #
    #-------------------------------------------------------------------------#
    zipFile = function(path, zipFiles) {

      rc <- zip(zipfile = path, files = zipFiles)
      if (rc == 0) {
        private$..state <- paste0('Successfully zipped ', basename(path), ".")
        private$..created <- Sys.time()
        private$..modified <- Sys.time()
        private$..accessed <- Sys.time()
        self$logIt()
      } else {
        private..state <- paste0("Unable to zip ", basename(path), ".")
        self$logIt('Error')
        stop()
      }
      invisible(self)
    },

    unZipFile = function(path, to, zipFiles = NULL, listFiles = FALSE,
                         overwrite = TRUE) {

      if (file.exists(path)) {
        unzip(zipfile = path, overwrite = overwrite, exdir = to,
              junkpaths = TRUE, files = zipFiles, list = listFiles)
        private$..state <-  paste0("Successfully unzipped ", basename(path), ".")
        private$..created <- Sys.time()
        private$..modified <- Sys.time()
        private$..accessed <- Sys.time()
        self$logIt()
      } else {
        private$..state <-  paste0("Could not unzip ", basename(path),
                                   ". File does not exist.")
        self$logIt('Error')
        stop()
      }
      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                         Move/Copy/Remove Methods                        #
    #-------------------------------------------------------------------------#
    moveFile = function(from, to)  {

      if (missing(from) | missing(to)) {
        private$..state <- paste('Missing parameters with no default. Usage is',
                                 'moveFile(from, to).  See ?File for',
                                 'further assistance.')
        self$logIt('Error')
        stop()
      }

      todir <- dirname(to)
      dir.create(todir, showWarnings = FALSE, recursive=TRUE)
      file.rename(from = from,  to = to)

      private$..state <-
        paste0("Successfully moved file(s) ", from, " to ", to, "." )
      self$logIt()

      invisible(self)
    },

    copyFile = function(from, to)  {

      if (missing(from) | missing(to)) {
        private$..state <- paste('Missing parameters with no default. Usage is',
                                 'copyFile(from, to).  See ?File for',
                                 'further assistance.')
        self$logIt('Error')
        stop()
      }
      todir <- dirname(to)
      if (!isTRUE(file.info(todir)$isdir))  dir.create(todir, recursive=TRUE)
      file.copy(from = from,  to = to, overwrite = TRUE)

      private$..state <-
        paste0("Successfully copied file ", from, " to ", to, "." )
      self$logIt()

      invisible(self)

    },

    removeFile = function(from) {

      if (missing(from)) {
        private$..state <- paste('Missing parameters with no default. Usage is',
                                 'removeFile(from).  See ?File for',
                                 'further assistance.')
        self$logIt("Error")
        stop()
      }
      file.remove(from)
      private$..state <- paste0("Successfully removed ", basename(from), ".")
      self$logIt()

      invisible(self)
    }
  )
)
