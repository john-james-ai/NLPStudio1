#==============================================================================#
#                             FileCollection                                   #
#==============================================================================#
#' FileCollection
#'
#' \code{FileCollection} Class containing a group of File objects.
#'
#' Class contains a group of file object and all methods required for reading, writing,
#' downloading, compressing, and repairing files.
#'
#' @section FileCollection Methods:
#'  \describe{
#'   \item{\code{new()}}{Creates an object of FileCollection Class}
#'   \item{\code{move(from, to)}}{Move a group of files.}
#'   \item{\code{copy(from, to)}}{Copy a group of files.}
#'   \item{\code{lock()}}{Makes a file collection read-only. }
#'   \item{\code{unlock()}}{Opens write access on a file collection. }
#'   \item{\code{read(io)}}{Reads a file collection.}
#'   \item{\code{write(io, content)}}{Writes a file collection.}
#'  }
#'
#' @section Parameters:
#' @param files List containing File objects.
#' @param io IO Object indicating the mode for reading and writing the file collection.
#' @param locked Boolean indicating whether a file collection is locked.
#' @param name A character string containing the name of the FileCollection object.
#' @param path Character string indicating the relative path for the File Collection object.
#' @param to Character string indicating the directory of file path to which a file collection is copied or moved.
#' @param url Character string containing the URL from which a file collection will be downloaded.
#' @param zipFiles Cheracter vector containing the names of files to be extracted.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
FileCollection <- R6::R6Class(
  classname = "FileCollection",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Corpus,

  private = list(
    files = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         FileCollection Core Methods                     #
    #-------------------------------------------------------------------------#
    initialize = function(name, path) {

      private$..className <- 'FileCollection'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..path <- path
      private$..io <- NULL
      private$..state <- paste("FileCollection object", private$..name, "instantiated.")
      private$..logs <- LogR$new()
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..accessed <- Sys.time()

      dir.create(private$..path, showWarnings = FALSE, recursive = TRUE)

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

      # Format download file path
      filePath <- file.path(private$..path, directory, fileName)

      if (download.file(url, destfile = file.path(directory, fileName), mode = 'wb') != 0) {
        private$..state <- paste0("Unable to download ", fileName, ".")
        self$logIt('Error')
        stop()
      }
      private$..state <- paste0("Successfully downloaded ", fileName, ". ")
      private$..created <- Sys.time()
      private$..modified <- Sys.time()
      private$..accessed <- Sys.time()
      self$logIt()

      return(filePath)
    },

    #-------------------------------------------------------------------------#
    #                            Access Methods                               #
    #-------------------------------------------------------------------------#
    lock = function() {
      private$..methodName <- 'lock'

      private$..files <- lapply(private$..files, function(f) {
        f$lock()
      })
      private$..state <- paste0("FileCollection object, ", private$..name, ", locked.")
      private$..modified <- Sys.time()
      self$logIt()
      invisible(self)
    },

    unlock = function() {
      private$..methodName <- 'lock'

      private$..files <- lapply(private$..files, function(f) {
        f$unlock()
      })
      private$..state <- paste0("FileCollection object, ", private$..name, ", unlocked.")
      private$..modified <- Sys.time()
      self$logIt()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Aggregate Methods                             #
    #-------------------------------------------------------------------------#
    getFiles = function() private$..files,

    addFile = function(file) {
      fileName <- file$getFileName()
      private$..files[[fileName]] <- file
    },

    removeFile = function(file) {
      fileName <- file$getFileName()
      private$..files[[fileName]] <- NULL
    },

    #-------------------------------------------------------------------------#
    #                               IO Methods                                #
    #-------------------------------------------------------------------------#
    read = function(io = NULL) {

      private$..methodName <- 'read'

      content <- lapply(private$..files, function(f) {
        f$read(io)
      })

      # LogIt
      private$..state <- paste0("Read ", private$..name, ". ")
      private$..accessed <- Sys.time()
      self$logIt()

      return(content)

    },

    write = function(content, io = NULL) {

      private$..methodName <- 'write'

      lapply(private$..files, function(f) {
        f$write(io, content)
      })

      # LogIt
      private$..state <- paste0("Wrote ", private$..name, ". ")
      private$..accessed <- Sys.time()
      self$logIt()

      invisible(self)

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

    unZipFile = function(path, directory, zipFiles = NULL, listFiles = FALSE,
                         overwrite = TRUE) {

      if (file.exists(path)) {
        unzip(zipfile = path, overwrite = overwrite,
              exdir = file.path(private..path, directory),
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
    moveFileCollection = function(to)  {

      private$..methodName <- 'moveFileCollection'


      if (missing(to)) {
        private$..state <- paste('Missing parameters with no default. Usage is',
                                 'moveFileCollection(to).  See ?FileCollection for',
                                 'further assistance.')
        self$logIt('Error')
        stop()
      }

      private$..files <- lapply(private$..files, function(f) {
        fileName <- f$getFileName()
        f$moveFile(file.path(to, fileName))
      })

      private$..path <- to

      private$..state <-
        paste0("Successfully moved file collection ", private$..path, " to ", to, "." )
      self$logIt()

      invisible(self)
    },

    removeFileCollection = function() {

      private$..methodName <- 'removeFileCollection'

      lapply(private$..files, function(f) {
        f$removeFile()
      })

      private$..state <- paste0("Successfully removed file collection, ", private$..name,  ".")
      self$logIt()

      invisible(self)
    }
  )
)
