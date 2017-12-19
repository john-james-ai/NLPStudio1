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
  inherit = Entity,

  private = list(
    ..filePaths = character(),
    ..locked = FALSE,
    ..path = character()
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
      private$..state <- paste("FileCollection object", private$..name, "instantiated.")
      private$..logs <- LogR$new()
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..accessed <- Sys.time()

      dir.create(private$..path, showWarnings = FALSE, recursive = TRUE)

      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                        Aggregate Methods                                #
    #-------------------------------------------------------------------------#
    getPath = function() private$..path,

    getFilePaths = function() private$..filePaths,

    addFilePath = function(filePath) {
      fileName <- basename(filePath)
      private$..filePaths[[fileName]] <- filepath
    },

    removeFilePath = function(filePath) {
      fileName <- basename(filePath)
      private$..filePaths[[fileName]] <- NULL
    },

    #-------------------------------------------------------------------------#
    #                          Download Method                                #
    #-------------------------------------------------------------------------#
    download = function(url) {

      private$..methodName <- 'download'

      # Format file path
      fileName <- installr::file.name.from.url(url)
      filePath <- file.path(private$..path, fileName)

      if (download.file(url, destfile = filePath, mode = 'wb') != 0) {
        private$..state <- paste0("Unable to download ", fileName, ".")
        self$logIt('Error')
        stop()
      }
      private$..filePaths <- filePath
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

      private$..state <- paste0("FileCollection object, ", private$..name, ", locked.")
      private$..modified <- Sys.time()
      self$logIt()
      invisible(self)
    },

    unlock = function() {
      private$..methodName <- 'lock'

      private$..locked <- FALSE

      private$..state <- paste0("FileCollection object, ", private$..name, ", unlocked.")
      private$..modified <- Sys.time()
      self$logIt()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                               IO Methods                                #
    #-------------------------------------------------------------------------#
    read = function(io = NULL) {

      private$..methodName <- 'read'

      files <- list.files(private$..path, full.names = TRUE)

      content <- lapply(files, function(f) {
        if (is.null(io))  io <- IOFactory$getIOStrategy(f)
        io$read(f)
      })

      # LogIt
      private$..state <- paste0("Read ", private$..name, ". ")
      private$..accessed <- Sys.time()
      self$logIt()

      return(content)
    },

    write = function(fileName, content, io = NULL) {

      private$..methodName <- 'write'

      if (private$..locked == TRUE) {
        private$..state <- paste0("Unable to write to ", private,
                                  ", the file collecdtion is locked.")
        self$logIt("Warn")
        stop()
      }

      filePath <- file.path(private$..path, fileName)

      if (is.null(io)) io <- IOFactory$getIOStrategy(filePath)

      io$write(filePath, content)

      # LogIt
      private$..state <- paste0("Wrote ", private$..name, ". ")
      private$..accessed <- Sys.time()
      self$logIt()

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

      dir.create(to, showWarnings = FALSE, recursive=TRUE)
      files <- list.files(private$..path, full.names = TRUE)
      lapply(files, function(f) {
        fileName <- basename(f)
        file.rename(from = f, to = file.path(to, fileName))
      })

      private$..state <-
        paste0("Successfully moved file collection ", private$..name, " from ",
               private$..path, " to ", to, "." )
      self$logIt()

      private$..path <- to

      invisible(self)
    },

    removeFileCollection = function() {

      private$..methodName <- 'removeFileCollection'

      files <- list.files(private$..path, full.names = TRUE)
      lapply(files, function(f) {
        file.remove(f)
      })

      private$..state <- paste0("Successfully removed file collection, ", private$..name,  ".")
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            Explose Object                               #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      o <- list(
        className	 =  private$..className ,
        name	 = 	    private$..name ,
        path	 = 	    private$..path ,
        filePaths = private$..filePaths,
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
