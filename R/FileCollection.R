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
    ..io = character(),
    ..files = list(),
    ..locked = FALSE,
    ..path = character(),

    parseInput = function() {

      path <- private$..path
      if (isDirectory(path)) {
        files <- list.files(path, full.names = TRUE)
      } else {
        wildcard <- basename(path)
        path <- dirname(path)
        files <-list.files(path = path, pattern = wildcard, full.names = TRUE)
      }
      return(files)
    }
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

      if (dir.exists(private$..path)) {
        self$loadFiles()
      } else {
        dir.create(private$..path, showWarnings = FALSE, recursive = TRUE)
      }

      self$logIt()

      invisible(self)
    },

    loadFiles = function() {

      private$..methodName <- 'loadFiles'

      files <- list.files(private$..path, full.names = TRUE)

      private$..files <- lapply(files, function(f) {
        name <- tools::file_path_sans_ext(basename(f))
        File$new(name = name, path = f)
      })
      names(private$..files) <- tools::file_path_sans_ext(basename(files))

      # Log it
      private$..state <- paste0("Loaded files into ", private$..name)
      self$logIt()
    },


    #-------------------------------------------------------------------------#
    #                        Aggregate Methods                                #
    #-------------------------------------------------------------------------#
    getPath = function() private$..path,

    getFiles = function() private$..files,

    addFile = function(file) {
      private$..methodName <- 'addFile'

      name <- file$getName()
      private$..files[[name]] <- file
      private$..state <- paste0("Added file ", name, ", to ", private$..name,
                                " file collection. ")
      private$..modified <- Sys.time()
      self$logIt()

      invisible(self)
    },

    removeFile = function(file) {
      private$..methodName <- 'removeFile'

      name <- getName(file)
      private$..files[[name]] <- NULL
      private$..state <- paste0("Removed file ", name, ", from ", private$..name,
                                " file collection. ")
      private$..modified <- Sys.time()
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            Access Methods                               #
    #-------------------------------------------------------------------------#
    lock = function() {
      private$..methodName <- 'lock'

      private$..locked <- TRUE

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

      private$..locked <- FALSE

      private$..files <- lapply(private$..files, function(f) {
        f$unLock()
      })

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

      content <- lapply(private$..files, function(f) {
        f$read(io)
      })

      # LogIt
      private$..state <- paste0("Read ", private$..name, ". ")
      private$..accessed <- Sys.time()
      self$logIt()

      return(content)
    },

    write = function(io = NULL) {

      private$..methodName <- 'write'

      if (private$..locked == TRUE) {
        private$..state <- paste0("Unable to write to ", private$..name,
                                  ", the file collection is locked.")
        self$logIt("Warn")
        stop()
      }

      lapply(private$..files, function(f) {
        f$write(io)
      })

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
        className	 =  private$..className ,
        name	 = 	    private$..name ,
        path	 = 	    private$..path ,
        files = private$..files,
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
