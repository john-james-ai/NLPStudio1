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
#' @section Cache methods:
#' \strong{Core Methods:}
#'  \itemize{
#'   \item{\code{new(object)}}{Method for instantiating a Cache object.}
#'   \item{\code{read()}}{Reads the object from the cache. }
#'   \item{\code{write()}}{Writes objects to the cache. Also performs eviction of objects when the maximum size of the cache has been reached. }
#'   \item{\code{maxSize()}}{Sets the maximum size of the file cache on disk.}
#'   \item{\code{trim(policy)}}{Trims the cache according to the policy parameter. Policy parameters are c("LFU", "LRU"), for least frequently used and least recently used, respectively}
#'   \item{\code{policy()}}{Sets trim policy. Valid value are c("LFU", "LRU"), for least frequently used and least recently used, respectively}
#'   \item{\code{print()}}{Prints the inventory of the cache.}
#'  }
#'
#' \strong{Other Methods:}
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Accepts a visitor object. Not implemented for this abstract class}
#'   \item{\code{logIt(level = 'Info')}}{Logs events relating to the document.}
#'  }
#'
#' @param id Character string containing the unique identifier for a cacheable object
#' @param content Content to be written to cache.
#' @param object Object to be written to, or read from the cache.
#' @param policy Character string indicating the trim policy. Valid values are c("LFU", "LRU"), for least frequently used and least recently used, respectively
#' @return data The cached payload content, if the read method is invoked or the original object sans payload.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Cache <- R6::R6Class(
  "SingletonContainer",
  portable = FALSE,
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Singleton,
  public = list(
    initialize = function(...) {
      Class <<- R6::R6Class(
        classname = "Cache",
        private = list(
          ..className = "Cache",
          ..methodName = character(),
          ..directory = ".R_cache",
          ..inventory = data.frame(),
          ..maxSize = 2000,
          ..currentSize = 0,
          ..trimPolicy = "LRU",
          ..item = list(
            id = character(),
            name = character(),
            size = numeric(),
            expired = FALSE,
            filePath = character(),
            created = character(),
            modified = character(),
            accessed = character(),
            nAccessed = numeric()
          ),

          trim = function(objectId, size) {

            private$..methodName <- "trim"

            while(private$..currentSize + size > private$..maxSize) {
              if (private$..trimPolicy == "LRU") {
                oldest <- subset(private$..inventory, subset(accessed == min(accessed) & id != objectId))
                expire <- subset(oldest, subset(size == max(size)))
                private$..inventory[id == expire$id[1], expired := TRUE]
                filePath <- expire$filePath[1]
                file.remove(filePath)
                private$..currentSize <- private$..currentSize - expire$size[1]
              } else {
                leastUsed <- subset(private$..inventory, subset(nAccessed == min(nAccessed) & id != objectId))
                expire <- subset(leastUsed, subset(size == max(size)))
                private$..inventory[id == expire$id[1], expired := TRUE]
                filePath <- expire$filePath[1]
                file.remove(filePath)
                private$..currentSize <- private$..currentSize - expire$size[1]
              }
            }
          },

          register = function(object, content) {

            private$..methodName <- 'register'

            objectId <- object$getId()

            if (nrow(private$..inventory[id == objectId]) == 0) {
              if (private$..maxSize < private$..currentSize + object.size(content)) {
                private$trim(objectId, object.size(content))
              }

              # Set item parameters
              private$..item$id <- id
              private$..item$name <- object$getName()
              private$..item$size <- object.size(content)
              private$..item$expired <- FALSE
              private$..item$filePath <- file.path(private$..directory,
                                                   paste0(class(object)[1],"-",
                                                          private$..item$name, "-",
                                                          private$..item$id, ".rdata"))
              private$..item$created  <- Sys.time()
              private$..item$modified <- Sys.time()
              private$..item$accessed <- Sys.time()
              private$..item$nAccessed <- 0

              # Add item to inventory
              private$..inventory <- rbind(as.data.frame(private$..item))

              # Return Filepath
              return(private$..item$filePath)

            } else {
              size <- subset(private$..inventory, subset(id == objectId), select(size))
              if (private$..maxSize < private$..currentSize + object.size(content) - size) {
                private$trim(objectId, object.size(content))
              }

              # Set item parameters
              private$..inventory[id == objectId, name := object$getName()]
              private$..inventory[id == objectId, size := object.size(content)]
              private$..inventory[id == objectId, expired := FALSE]
              private$..inventory[id == objectId, filePath :=
                                    file.path(private$..directory,
                                              paste0(class(object)[1],"-",
                                                     private$..item$name, "-",
                                                     private$..item$id, ".rdata"))]
              private$..inventory[id == objectId, modified := Sys.time()]

              # Return filepath
              return(subset(private$..inventory, subset(id == objectId), select(filePath)))
            }
          }
        ),
        active = list(
          maxSize = function(value) {

            private$..methodName <- "maxSize"

            if (missing(value)) {
              return(private$..maxSize)
            } else {
              if (class(value) != "integer") {

                private$..state <- paste0("Maximum size should be an integer. ",
                                          "See ?", class(self)[1], " for ",
                                          "further assistance. ")
                self$logIt("Error")
                stop()
              } else if (value < 1000 | value > 10000) {
                private$..state <- paste("Maximum cache size must be integer and",
                                         "between 1000 and 10000 megabytes. ",
                                         "See ?", class(self)[1], " for ",
                                         "further assistance. ")
                self$logIt("Error")
                stop()
              } else {
                private$..maxSize <- value
              }
            }
          }
        ),

        public = list(

          #-------------------------------------------------------------------------#
          #                           Core Methods                                  #
          #-------------------------------------------------------------------------#
          initialize = function() {

            # Set log parameters
            private$..className <- "Cache"
            private$..methodName <- "initialize"
            private$..logs <- LogR$new()

            # Create database folder (if it doesn't already exists)
            dir.create(private$..directory, showWarnings = FALSE, recursive = TRUE)

            # Log it
            private$..state <- paste0("Cache object for ", private$..item$name, ", instantiated.")
            self$logIt()

            invisible(self)
          },

          #-------------------------------------------------------------------------#
          #                             IO Methods                                  #
          #-------------------------------------------------------------------------#
          read = function(object) {

            private$..methodName <- 'read'

            #TODO: check for expiration

            id <- object$getId()

            if (is.null(private$..inventory[[id]])) {
              private$..state <- paste0("Cache object, ", id, "does not exist.",
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
            private$..state <- paste0("Read ", item$name, " from cache. ")
            self$logIt()

            return(data)
          },

          write = function(object, content) {

            private$..methodName <- 'write'

            # Register write
            filePath <- private$register(object, content)

            # Write to cache
            io <- IOFactory$new(filePath)$getIOStrategy()
            io$write(path = filePath, content = content)

            # Log
            private$..state <- paste0("Wrote ", object$getName(), " to cache. ")
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
            documentCache <- list(
              object <- private$..object,
              path <- private$..path,
              state = private$..state,
              created = private$..created,
              modified = private$..modified,
              accessed = private$..accessed
            )
            return(documentCache)
          }
        )
      )
      super$initialize(...)
    }
  )
)#$new()
