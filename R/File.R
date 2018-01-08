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
#'   \item{\code{read(io)}}{Reads a File.}
#'   \item{\code{write(io, content)}}{Writes a File.}
#'  }
#'
#' @section Parameters:
#' @param io IO Object indicating the mode for reading and writing the File.
#' @param name A character string containing the name of the File object.
#' @param path Character string indicating the relative path, including the file name, for the File object
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
    ..io  = character(),
    ..fileName = character(),
    ..fileSize = numeric(),
    ..fileFormat = character(),
    ..content = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Instantiation Method                          #
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

      invisible(self)
    },
    getContent = function() private$..content,
    setContent = function(content) {
      private$..content <- content
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                               IO Methods                                #
    #-------------------------------------------------------------------------#
    loadFile = function(io = NULL) {

      private$..methodName <- 'loadFile'

      if (is.null(io)) io <-  private$..io

      # Read content
      private$..content <- io$read(private$..path)

      # Update file meta data
      private$..fileSize <- file.size(private$..path)
      private$..fileFormat <- ifelse(class(private$..content) == 'raw', "bin", tools::file_ext(private$..fileName))
      private$..created <- file.info(private$..path)[,'ctime']
      private$..modified <- file.info(private$..path)[,'mtime']
      private$..accessed <- Sys.time()

      # LogIt
      private$..state <- paste0("Loaded file ", private$..name, ". ")
      self$logIt()

      invisible(self)
    },

    saveFile = function(io = NULL) {

      private$..methodName <- 'saveFile'

      # Write content to file
      if (is.null(io)) io <- private$..io
      io$write(private$..path, private$..content)

      # Update meta data
      private$..fileSize <- file.size(private$..path)
      private$..fileFormat <- ifelse(class(private$..content) == 'raw', "bin", tools::file_ext(private$..fileName))
      private$..created <- file.info(private$..path)[,'ctime']
      private$..modified <- file.info(private$..path)[,'mtime']
      private$..accessed <- file.info(private$..path)[,'atime']

      # LogIt
      private$..state <- paste0("Saved file ", private$..name, ". ")
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

      file <- list(
        name = private$..name,
        path = private$..path,
        created = private$..created,
        modified = private$..modified,
        accessed = private$..accessed,
        className = private$..className,
        methodName = private$..methodName,
        state = private$..state,
        logs = private$..logs,
        io = private$..io,
        fileName = private$..fileName,
        fileSize = private$..fileSize,
        fileFormat = private$..fileFormat,
        content = private$..content
        )
      return(file)
    }
  )
)
