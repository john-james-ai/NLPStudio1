#==============================================================================#
#                                  Cache                                       #
#==============================================================================#
#' Cache
#'
#' \code{Cache} Class responsible for managing, retrieving from and writing to the cache.
#'
#' Class maintains persistence for large memory intensive cacheable components
#' of objects on NLPStudio. Cacheable objects are characterized by a payload
#' component which contains the domain content for the object, text for example,
#' and a metadata component that contains information about the payload. This
#' class is responsible for caching of the payload components of the objects.
#' The metadata components are retained in memory.  Object payload components
#' are stored in the .R_cache subdirectory of the current working
#' directory. An inventory of "tenants" in the cache is maintained and the size
#' is controlled based upon a designated maximum size and a least recently
#' used (LRU) eviction strategy.
#'
#' @template cacheClasses.R
#'
#' @section Cache methods:
#' \strong{Core Methods:}
#'  \itemize{
#'   \item{\code{new(object)}}{Method for instantiating a Cache object.}
#'   \item{\code{read()}}{Reads the object from the cache. }
#'   \item{\code{write()}}{Writes objects to the cache. Also performs eviction of objects when the maximum size of the cache has been reached. }
#'  }
#'
#' \strong{Other Methods:}
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Accepts a visitor object. Not implemented for this abstract class}
#'   \item{\code{logIt(level = 'Info')}}{Logs events relating to the document.}
#'  }
#'
#' @section Parameters:
#' @param object Object to be written to, or read from the cache.
#' @return data The cached payload content, if the read method is invoked or the original object sans payload.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Cache <- R6::R6Class(
  classname = "Cache",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..directory = ".R_cache",
    ..inventory = list(),
    ..maxSize = numeric(),
    ..currentSize = numeric(),
    ..item = list(
      id = character(),
      expired = logical(),
      filePath = character(),
      factory = character(),
      input = character(),
      created = character(),
      modified = character(),
      accessed = character(),
      nAccessed = numeric()
    )
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
      private$..className <- 'Cache'
      private$..methodName <- 'initialize'
      private$..state <- paste0("Cache, ", private$..fileName, ", instantiated.")
      private$..logs <- LogR$new()
      private$..created <- Sys.time()
      private$..modified <- Sys.time()
      private$..accessed <- Sys.time()

      # Create log entry
      self$logIt()

      invisible(self)
    },

    getCacheName = function() private$..fileName,

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
