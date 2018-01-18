#==============================================================================#
#                                  CacheObject                                       #
#==============================================================================#
#' CacheObject
#'
#' \code{CacheObject} Class responsible for managing, retrieving from and writing to the CacheObject.
#'
#' Class maintains persistence for large memory intensive CacheObjectable components
#' of objects on NLPStudio. CacheObjectable objects are characterized by a payload
#' component which contains the domain content for the object, text for example,
#' and a metadata component that contains information about the payload. This
#' class is responsible for caching of the payload components of the objects.
#' The metadata components are retained in memory.  Object payload components
#' are stored in the .R_CacheObject subdirectory of the current working
#' directory. An inventory of "tenants" in the CacheObject is maintained and the size
#' is controlled based upon a designated maximum size and a least recently
#' used (LRU) eviction strategy.
#'
#' @section CacheObject methods:
#' \strong{Core Methods:}
#'  \itemize{
#'   \item{\code{new(object)}}{Method for instantiating a CacheObject object.}
#'   \item{\code{read()}}{Reads the object from the CacheObject. }
#'   \item{\code{write()}}{Writes objects to the CacheObject. Also performs eviction of objects when the maximum size of the CacheObject has been reached. }
#'   \item{\code{maxSize()}}{Sets the maximum size of the file CacheObject on disk.}
#'   \item{\code{trim(policy)}}{Trims the CacheObject according to the policy parameter. Policy parameters are c("LFU", "LRU"), for least frequently used and least recently used, respectively}
#'   \item{\code{policy()}}{Sets trim policy. Valid value are c("LFU", "LRU"), for least frequently used and least recently used, respectively}
#'   \item{\code{print()}}{Prints the inventory of the CacheObject.}
#'  }
#'
#' \strong{Other Methods:}
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Accepts a visitor object. Not implemented for this abstract class}
#'   \item{\code{logIt(level = 'Info')}}{Logs events relating to the document.}
#'  }
#'
#' @param id Character string containing the unique identifier for a CacheObjectable object
#' @param content Content to be written to CacheObject.
#' @param object Object to be written to, or read from the CacheObject.
#' @param policy Character string indicating the trim policy. Valid values are c("LFU", "LRU"), for least frequently used and least recently used, respectively
#' @return data The CacheObjectd payload content, if the read method is invoked or the original object sans payload.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Class <- R6::R6Class(
  classname = "CacheObject",
  inherit = Entity,
  lock_objects = FALSE,
  lock_class = FALSE,
  private = list(
    ..className = "CacheObject",
    ..methodName = character(),
    ..id = character(),
    ..name = character(),
    ..size = numeric(),
    ..expired = FALSE,
    ..filePath = character(),
    ..created = character(),
    ..modified = character(),
    ..accessed = character(),
    ..nAccessed = numeric()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Core Methods                                #
    #-------------------------------------------------------------------------#

    initialize = function(object) {

      # Set log parameters
      private$..methodName <- "initialize"
      private$..logs <- LogR$new()

      # Format Cache Object
      id <- object$getId()
      name <- object$getName()
      filePath <- file.path(private$..directory,
                            paste0(class(object)[1],"-", name, "-",
                                   id, ".rdata"))

      # Log it
      private$..state <- paste0("CacheObject object for ", private$..item$name, ", instantiated.")
      self$logIt()

      invisible(self)
    },

    getInstance = function() invisible(self),

    #-------------------------------------------------------------------------#
    #                             IO Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(object) {

      private$..methodName <- 'read'

      #TODO: check for expiration

      id <- object$getId()

      if (is.null(private$..inventory[[id]])) {
        private$..state <- paste0("CacheObject object, ", id, "does not exist.",
                                  "See ?", class(self)[1], " for ",
                                  "further assistance. ")
        self$logIt("Error")
        stop()
      }

      # Read data
      item <- private$..inventory[[id]]
      io <- IOFactory$new(item$filePath)$getIOStrategy()
      data <- io$read(item$filePath)

      # Update metadata
      private$..item[[id]]$accessed <- Sys.time()
      private$..item[[id]]$nAccessed <- private$..item[[id]]$nAccessed + 1

      # Log
      private$..state <- paste0("Read ", item$name, " from CacheObject. ")
      self$logIt()

      return(data)
    },

    write = function(object, content) {

      private$..methodName <- 'write'

      private$save(object, content)
      private$register(object)
      private$trim(object)

      # Log
      private$..state <- paste0("Saved ", object$getName(), " to CacheObject. ")
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$CacheObject(self)
    },

    #-------------------------------------------------------------------------#
    #                            Test Methods                                 #
    #-------------------------------------------------------------------------#
    exposeObject = function() {
      documentCacheObject <- list(
        inventory = private$..inventory,
        maxSize = private$..maxSize,
        currentSize = private$..currentSize,
        trimPolicy = private$..trimPolicy,
        item = private$..item
      )
      return(documentCacheObject)
    }
  )
)
