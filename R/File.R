#==============================================================================#
#                                  File                                        #
#==============================================================================#
#' File
#'
#' \code{File} Class responsible for managing data stored to disk
#'
#' @section File methods:
#' \strong{Core Methods:}
#'  \itemize{
#'   \item{\code{new(object)}}{Method for instantiating a File object.}
#'   \item{\code{read()}}{Reads content from the database object. }
#'   \item{\code{write(content)}}{Method for writing content to the database object. }
#'   \item{\code{getName()}}{Method returns the name of database object}
#'  }
#'
#' \strong{Other Methods:}
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Accepts a visitor object. Not implemented for this abstract class}
#'   \item{\code{logIt(level = 'Info')}}{Logs events relating to the document.}
#'  }
#'
#' @section Parameters:
#' @param object Object containing the stored data
#' @param content List containing character vectors of text.
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
    ..directory = "./NLPStudio/database",
    ..fileName = character(),
    ..object = character(),
    ..path = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(object) {

      # Extract data from object parameters
      name <- object$getName()
      className <- class(object)[1]
      timeStamp <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
      private$..fileName <- paste0(className, "-", name, "-", timeStamp, ".RData")
      private$..path <- file.path(private$..directory, private$..fileName)
      private$..object <- object

      # Create database folder (if it doesn't already exists)
      dir.create(private$..directory, showWarnings = FALSE, recursive = TRUE)

      # Instantiate variables
      private$..className <- 'File'
      private$..methodName <- 'initialize'
      private$..state <- paste0("File, ", private$..fileName, ", instantiated.")
      private$..logs <- LogR$new()
      private$..created <- Sys.time()
      private$..modified <- Sys.time()
      private$..accessed <- Sys.time()

      # Create log entry
      self$logIt()

      invisible(self)
    },

    getFileName = function() private$..fileName,

    #-------------------------------------------------------------------------#
    #                             IO Methods                                  #
    #-------------------------------------------------------------------------#
    read = function() {

      private$..methodName <- 'read'

      # Set read method
      io <- IOFactory$new(private$..path)$getIOStrategy()

      # Update metadata
      private$..accessed <- Sys.time()

      # Log
      private$..state <- paste0("Read ", private$..object$getName(), ". ")
      self$logIt()

      return(io$read(private$..path))
    },

    write = function(content) {

      private$..methodName <- 'write'

      # Set write method
      io <- IOFactory$new(private$..path)$getIOStrategy()

      # Update meta data
      if (!file.exists(private$..path)) private$..created <- Sys.time()
      private$..modified <- Sys.time()
      private$..accessed <- Sys.time()

      # Write
      io$write(path = private$..path, content = content)

      # Log
      private$..state <- paste0("Wrote ", private$..object$getName(), ". ")
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$database(self)
    },

    #-------------------------------------------------------------------------#
    #                            Test Methods                                 #
    #-------------------------------------------------------------------------#
    exposeObject = function() {
      databaseObject <- list(
        object <- private$..object,
        path <- private$..path,
        state = private$..state,
        created = private$..created,
        modified = private$..modified,
        accessed = private$..accessed
      )
      return(databaseObject)
    }
  )
)
