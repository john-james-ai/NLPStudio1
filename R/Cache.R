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
  classname = "Cache",
  inherit = Entity,
  lock_objects = FALSE,
  lock_class = FALSE,
  private = list(
    ..className = "Cache",
    ..methodName = character(),
    ..cacheDir = ".R_Cache",
    ..cacheStateFile = "cacheState.rdata",
    ..cacheState = list(
      settings = list(
        maxSize = 2000,
        currentSize = 0,
        trimPolicy = "LRU"
      ),
      inventory = data.table(),
      item = list(
        id = character(),
        name = character(),
        size = numeric(),
        expired = FALSE,
        filePath = character(),
        created = character(),
        modified = character(),
        accessed = character(),
        nAccessed = numeric()
      )
    ),

    getCacheState = function() {

      private$..methodName <- "getCacheState"

      # Obtain cache state
      cacheStatePath <- file.path(private$..cacheDir, private$..cacheStateFile)

      if (file.exists(cacheStatePath)) {
        io <- IOFactory$new()$getIOStrategy(cacheStatePath)
        private$..cacheState <- io$read(cacheStatePath)
      }

      # Sync cache state with file directory
      files <- list.files(private$..cacheDir, all.files = TRUE,
                            full.names = TRUE, recursive = TRUE)
      for (i in 1:length(files)) {

        # Obtain cache state for file
        fileName <- basename(files[i])
        fileName <- strsplit(fileName, split = "-")
        private$..cacheState$item$id <- fileName[[1]][3]
        private$..cacheState$item$name <- fileName[[1]][2]
        private$..cacheState$item$size <- file.size(files[i]) / 1000000
        private$..cacheState$item$expired <- FALSE
        private$..cacheState$item$filePath <- files[i]
        private$..cacheState$item$created <- file.info(files[i])$ctime
        private$..cacheState$item$modified <- file.info(files[i])$mtime
        private$..cacheState$item$accessed <- file.info(files[i])$atime
        private$..cacheState$item$nAccessed <- 0

        # Get the number of times the file was accessed
        if(nrow(private$..cacheState$inventory) > 0) {
          if(nrow(subset(private$..cacheState$inventory, id == fileName[[1]][3])) > 0) {
            private$..cacheState$item$nAccessed <-
              subset(private$..cacheState$inventory, id == fileName[[1]][3],
                     select = nAccessed)
            private$..cacheState$inventory <-
              private$..cacheState$inventory[private$..cacheState$inventory$id != fileName[[1]][3]]
          }
        }

        # Update row
        private$..cacheState$inventory <- rbind(as.data.frame(private$..cacheState$item))
      })

      private$..cacheState$settings$currentSize <-
        sum(file.info(files)$size) / 1000000
    },

    putCacheState = function() {

      private$..methodName <- "putCacheState"
      cacheStatePath <- file.path(private$..cacheDir, private$..cacheStateFile)
      io <- IOFactory$new()$getIOStrategy(cacheStatePath)
      io$write(path = cacheStatePath, content = private$..cacheState)

    },

    getFilePath = function(object) {
      private$..methodName <- "getFilePath"
      id <- object$getId()
      name <- object$getName()
      filePath <- file.path(private$..cacheDir,
                            paste0(class(object)[1],"-",
                                   name, "-",
                                   id, ".rdata"))
      return(filePath)
    },

    checkFileExists = function(filePath) {
      private$..methodName <- "checkFileExists"
      fileName <- basename(filePath)
      if (!file.exists(filePath)) {
        private$..state <- paste0("Cache object, ", fileName, "does not exist.",
                                  "See ?", class(self)[1], " for ",
                                  "further assistance. ")
        self$logIt("Error")
        stop()
      }
    },

    logReadCache = function(object) {
      private$..methodName <- "logReadCache"
      objectId <- object$getId()
      private$..cacheState$inventory[id == objectId, accessed := Sys.time()]
      private$..cacheState$inventory[id == objectId, nAccessed := nAccessed + 1]
      private$putCacheState()
    },

    logWriteCache = function(object) {
      private$..methodName <- "logWriteCache"
      objectId <- object$getId()
      name <- object$getName()
      filePath <- private$getFilePath(object)
      fileSize <- file.size(filePath) / 1000000
      if (is.null(subset(private$..cacheState$inventory, id == objectId))) {
        private$..cacheState$settings$currentSize <-
          private$..cacheState$settings$currentSize + fileSize
        private$..cacheState$item$id <- objectId
        private$..cacheState$item$name <- name
        private$..cacheState$item$size <- fileSize
        private$..cacheState$item$expired <- FALSE
        private$..cacheState$item$filePath <- filePath
        private$..cacheState$item$created <- Sys.time()
        private$..cacheState$item$modified <- Sys.time()
        private$..cacheState$item$accessed <- Sys.time()
        private$..cacheState$item$nAccessed <- 0
        private$..cacheState$inventory <-
          rbind(as.data.frame(private$..cacheState$item))
        private$putCacheState()
      } else {
        priorSize <- subset(private$..cacheState, id == objectId, select = size)
        private$..cacheState$settings$currentSize <-
          private$..cacheState$settings$currentSize + fileSize - priorSize
        private$..cacheState$inventory[id == objectId, name := name]
        private$..cacheState$inventory[id == objectId, size := fileSize]
        private$..cacheState$inventory[id == objectId, expired := FALSE]
        private$..cacheState$inventory[id == objectId, filePath := filePath]
        private$..cacheState$inventory[id == objectId, updated := updated]
        private$..cacheState$inventory[id == objectId, accessed := accessed]
        private$..cacheState$inventory[id == objectId, nAccessed := nAccessed + 1]
        private$putCacheState()
      }
      return(private$..cacheState$inventory[id == objectId])
    },

    save = function(object, content) {

      private$..methodName <- 'save'
      filePath <- private$getFilePath(object)
      io <- IOFactory$new(filePath)$getIOStrategy()
      io$write(path = filePath, content = content)
    },

    trim = function(object) {

      private$..methodName <- "trim"

      objectId <- object$getId()

      print("*******************************************")
      print(paste("Current size:", private$..cacheState$settings$currentSize))
      print(paste("Maxmum size:", private$..cacheState$settings$maxSize))

      while(private$..cacheState$settings$currentSize  >
            private$..cacheState$settings$maxSize) {
        print("**** TRIMMING ")
        print(paste("Current size:", private$..cacheState$settings$currentSize))
        print(paste("Maxmum size:", private$..cacheState$settings$maxSize))
        if (private$..cacheState$settings$trimPolicy == "LRU") {
          oldest <- subset(private$..cacheState$inventory, accessed == min(accessed) & id != objectId)
          expire <- subset(oldest, size == max(size))

        } else {
          leastUsed <- subset(private$..cacheState$inventory, nAccessed == min(nAccessed) & id != objectId)
          expire <- subset(leastUsed, size == max(size))
        }
        private$..cacheState$inventory[id == expire[1]$id, size := 0]
        private$..cacheState$inventory[id == expire[1]$id, expired := TRUE]
        private$..cacheState$inventory[id == expire[1]$id, filePath := NULL]
        private$..cacheState$inventory[id == expire[1]$id, modified := Sys.time()]
        private$..cacheState$inventory[id == expire[1]$id, accessed := Sys.time()]
        private$..cacheState$inventory[id == expire[1]$id, nAccessed := 0]
        filePath <- expire[1]$filePath
        file.remove(filePath)
        private$..currentSize <- private$..currentSize - expire[1]$size
      }
      private$putCacheState()
    }
  ),

  active = list(
    maxSize = function(value) {

      private$..methodName <- "maxSize"

      if (missing(value)) {
        return(private$..cacheState$settings$maxSize)
      } else {
        if (class(value) != "numeric") {

          private$..state <- paste0("Maximum size should be an numeric value. ",
                                    "See ?", class(self)[1], " for ",
                                    "further assistance. ")
          self$logIt("Error")
          stop()
        } else if (value < 1000 | value > 10000) {
          private$..state <- paste0("Maximum CacheManager size must be integer and ",
                                    "between 1000 and 10000 megabytes. ",
                                    "See ?", class(self)[1], " for ",
                                    "further assistance. ")
          self$logIt("Error")
          stop()
        } else {
          private$..cacheState$settings$maxSize <- value
        }
      }
    },

    policy = function(value) {

      private$..methodName <- "policy"

      if (missing(value)) {
        return(private$..cacheState$settings$trimPolicy)
      } else {
        if (!(value) %in% c("LFU", "LRU")) {
          private$..state <- paste0("Invalid trim policy. Valid values are ",
                                    "c('LRU', 'LFU'), for least recently used ",
                                    "and least frequently used, respectively. ",
                                    "See ?", class(self)[1], " for ",
                                    "further assistance. ")
          self$logIt("Error")
          stop()
        } else {
          private$..cacheState$settings$trimPolicy <- value
        }
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Core Methods                                #
    #-------------------------------------------------------------------------#

    initialize = function() {

      # Set log parameters
      private$..className <- "Cache"
      private$..methodName <- "initialize"
      private$..logs <- LogR$new()

      # Create database folder (if it doesn't already exists)
      dir.create(private$..cacheDir, showWarnings = FALSE, recursive = TRUE)

      # Obtain cache state, if it exists
      private$getCacheState()

      # Log it
      private$..state <- paste0("Cache object for ", private$..item$name, ", instantiated.")
      self$logIt()

      invisible(self)
    },

    getSettings = function() {
      private$getCacheState()
      return(private$..cacheState$settings)
    },

    getInventory = function() {
      private$getCacheState()
      return(private$..cacheState$inventory)
    },

    #-------------------------------------------------------------------------#
    #                             IO Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(object) {

      private$..methodName <- 'read'

      # Confirm file exists
      filePath <- private$getFilePath(object)
      private$checkFileExists(filePath)

      # Read data
      io <- IOFactory$new(filePath)$getIOStrategy()
      data <- io$read(filePath)

      # Update cache state
      private$logReadCache(object)

      # Log
      private$..state <- paste0("Read ", object$getName(), " from cache. ")
      self$logIt()

      return(data)
    },

    write = function(object, content) {

      private$..methodName <- 'write'

      # Save the file to cache
      private$save(object, content)

      # Trim if cache is full
      private$trim(object)

      # Update cache state
      cache <- private$logWriteCache(object)

      # Log
      private$..state <- paste0("Saved ", object$getName(), " to cache. ")
      self$logIt()

      invisible(cache)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$cache(self)
    },

    #-------------------------------------------------------------------------#
    #                            Test Methods                                 #
    #-------------------------------------------------------------------------#
    exposeObject = function() {
      documentCache <- list(
        inventory = private$..inventory,
        maxSize = private$..maxSize,
        currentSize = private$..currentSize,
        trimPolicy = private$..trimPolicy,
        item = private$..item
      )
      return(documentCache)
    }
  )
)
