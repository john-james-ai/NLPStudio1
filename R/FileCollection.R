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

    parseInput = function() {

      path <- private$..admin$path
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

      private$..admin$className <- 'FileCollection'
      private$..admin$methodName <- 'initialize'
      private$..name <- name
      private$..admin$path <- path
      private$..admin$state <- paste("FileCollection object", private$..name, "instantiated.")
      private$..admin$logs <- LogR$new()
      private$..admin$modified <- Sys.time()
      private$..admin$created <- Sys.time()
      private$..admin$accessed <- Sys.time()

      if (file.exists(private$..admin$path)) {
        files <- list.files(private$..admin$path, full.names = TRUE)
        private$..files <- lapply(files, function(f) {
          name = tools::file_path_sans_ext(basename(f))
          File$new(name = name, path = f)
        })
      }

      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                        Aggregate Methods                                #
    #-------------------------------------------------------------------------#
    getPath = function() private$..admin$path,

    getFiles = function() private$..files,

    addFile = function(file) {
      private$..admin$methodName <- 'addFile'

      name <- file$getName()
      private$..files[[name]] <- file
      private$..admin$state <- paste0("Added file ", name, ", to ", private$..name,
                                " file collection. ")
      private$..admin$modified <- Sys.time()
      self$logIt()

      invisible(self)
    },

    removeFile = function(file) {
      private$..admin$methodName <- 'removeFile'

      name <- getName(file)
      private$..files[[name]] <- NULL
      private$..admin$state <- paste0("Removed file ", name, ", from ", private$..name,
                                " file collection. ")
      private$..admin$modified <- Sys.time()
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            Access Methods                               #
    #-------------------------------------------------------------------------#
    lock = function() {
      private$..admin$methodName <- 'lock'

      private$..admin$locked <- TRUE

      private$..files <- lapply(private$..files, function(f) {
        f$lock()
      })

      private$..admin$state <- paste0("FileCollection object, ", private$..name, ", locked.")
      private$..admin$modified <- Sys.time()
      self$logIt()
      invisible(self)
    },

    unlock = function() {
      private$..admin$methodName <- 'lock'

      private$..admin$locked <- FALSE

      private$..files <- lapply(private$..files, function(f) {
        f$unLock()
      })

      private$..admin$state <- paste0("FileCollection object, ", private$..name, ", unlocked.")
      private$..admin$modified <- Sys.time()
      self$logIt()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                               IO Methods                                #
    #-------------------------------------------------------------------------#
    read = function(io = NULL) {

      private$..admin$methodName <- 'read'

      content <- lapply(private$..files, function(f) {
        f$read(io)
      })

      # LogIt
      private$..admin$state <- paste0("Read ", private$..name, ". ")
      private$..admin$accessed <- Sys.time()
      self$logIt()

      return(content)
    },

    write = function(io = NULL) {

      private$..admin$methodName <- 'write'

      if (private$..admin$locked == TRUE) {
        private$..admin$state <- paste0("Unable to write to ", private$..name,
                                  ", the file collection is locked.")
        self$logIt("Warn")
        stop()
      }

      lapply(private$..files, function(f) {
        f$write(io)
      })

      # LogIt
      private$..admin$state <- paste0("Wrote ", private$..name, ". ")
      private$..admin$accessed <- Sys.time()
      self$logIt()

      invisible(self)
    },

    flush = function() {
      lapply(private$..files, function(f) {
        f$flush()
      })
    },


    #-------------------------------------------------------------------------#
    #                            Explose Object                               #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      o <- list(
        className	 =  private$..admin$className ,
        name	 = 	    private$..name ,
        path	 = 	    private$..admin$path ,
        files = private$..files,
        state	 = 	    private$..admin$state ,
        logs	 = 	    private$..admin$logs ,
        modified	 = 	private$..admin$modified ,
        created	 = 	  private$..admin$created ,
        accessed	 = 	private$..admin$accessed
      )
      return(o)
    }

  )
)
