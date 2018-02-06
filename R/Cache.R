#==============================================================================#
#                                 NLPStudio                                    #
#==============================================================================#
#' NLPStudio
#'
#' \code{NLPStudio} Containing class containing NLP data and modeling pipelines.
#'
#' This class creates and manages data science pipelines. Multiple pipelines
#' can be created, with separate data sets, feature engineering, and models.
#'
#' \strong{NLPStudio Core Methods:}
#' \describe{
#'  \item{\code{new()}}{Initializes the NLPStudio as s singleton object at load time.}
#'
#'  \item{\code{getInstance()}}{Returns the current NLPStudio instance object. This will be the only instantiation called "nlpStudio.},
#' }
#'
#'
#'
#' @docType class
#'
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
          ..state = character(),
          ..logs = character(),
          ..created = character(),
          ..modified = character(),
          ..accessed = character(),
          ..cacheDir = ".R_Cache",
          ..cacheStateFile = "cacheState.RData",
          ..cacheState = list(
            settings = list(
              maxSize = 2000,
              currentSize = 0,
              trimPolicy = "LRU"
            ),
            inventory = data.table::data.table(),
            item = list(
              id = character(),
              size = numeric(),
              expired = FALSE,
              filePath = character(),
              created = character(),
              modified = character(),
              accessed = character(),
              nAccessed = numeric()
            )
          ),

          updateCacheInventoryFromFile = function() {

            private$..methodName <- "updateCacheInventoryFromFile"

            private$..cacheState$inventory[id == private$..cacheState$item$id, size := private$..cacheState$item$size]
            private$..cacheState$inventory[id == private$..cacheState$item$id, expired := private$..cacheState$item$expired]
            private$..cacheState$inventory[id == private$..cacheState$item$id, filePath := private$..cacheState$item$filePath]
            private$..cacheState$inventory[id == private$..cacheState$item$id, created := private$..cacheState$item$created]
            private$..cacheState$inventory[id == private$..cacheState$item$id, modified := private$..cacheState$item$modified]
            private$..cacheState$inventory[id == private$..cacheState$item$id, accessed := private$..cacheState$item$accessed]
            private$putCacheState()
          },

          addCacheInventoryFromFile = function() {
            item <- as.data.table(private$..cacheState$item)
            private$..cacheState$inventory <- rbind(private$..cacheState$inventory,
                                                    item)
            private$putCacheState()
          },

          getCacheState = function(reset = FALSE) {

            private$..methodName <- "getCacheState"

            # Obtain cache state
            if (file.exists(private$..cacheStateFile) & reset == FALSE) {
              io <- IOFactory$new(private$..cacheStateFile)$getIOStrategy()
              private$..cacheState <- io$read(private$..cacheStateFile)
            }

            if (reset == TRUE) {
              private$..cacheState$inventory <- data.table::data.table()
            }

            # Sync cache state with file directory
            files <- list.files(private$..cacheDir, all.files = TRUE,
                                full.names = TRUE, recursive = TRUE)
            if (length(files) > 0) {
              for (i in 1:length(files)) {

                # Get id from cache file name
                fileName <- tools::file_path_sans_ext(basename(files[i]))
                id <- sub('.*\\-', '', fileName)

                private$..cacheState$item$id <- id
                private$..cacheState$item$size <- private$getFileSize(files[i])
                private$..cacheState$item$expired <- FALSE
                private$..cacheState$item$filePath <- files[i]
                private$..cacheState$item$created <- file.info(files[i])$ctime
                private$..cacheState$item$modified <- file.info(files[i])$mtime
                private$..cacheState$item$accessed <- file.info(files[i])$atime
                private$..cacheState$item$nAccessed <- 0

                # Process update if entry exists in cache inventory table, otherwise add
                if (nrow(private$..cacheState$inventory) > 0) {
                  if (nrow(subset(private$..cacheState$inventory,
                                  id == private$..cacheState$item$id)) > 0) {
                    private$updateCacheInventoryFromFile()
                  } else {
                    private$addCacheInventoryFromFile()
                  }
                } else {
                  private$addCacheInventoryFromFile()
                }
              }

              private$..cacheState$settings$currentSize <-
                sum(file.info(files)$size) / 1000000
            }

            # Expire files not in cache
            private$..cacheState$inventory[!(filePath %in% files), expired := TRUE]
            private$..cacheState$inventory[!(filePath %in% files), modified := Sys.time()]

            return(private$..cacheState)
          },

          putCacheState = function() {

            private$..methodName <- "putCacheState"
            io <- IOFactory$new(private$..cacheStateFile)$getIOStrategy()
            io$write(path = private$..cacheStateFile, content = private$..cacheState)

          },

          getFileSize = function(filePath) {
            return(file.size(filePath) / 1000000)
          },

          getFilePath = function(object) {
            private$..methodName <- "getFilePath"
            id <- object$getId()
            filePath <- file.path(private$..cacheDir,
                                  paste0(class(object)[1],"-",
                                         id, ".RData"))
            return(filePath)
          },

          checkFileExists = function(filePath) {
            private$..methodName <- "checkFileExists"
            fileName <- basename(filePath)
            if (!file.exists(filePath)) {
              private$..state <- paste0("Cache object, ", fileName, " does not exist. ",
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

            # Obtain key variables
            objectId <- object$getId()
            filePath <- private$getFilePath(object)
            size <- private$getFileSize(filePath)
            private$..cacheState$item$id <- objectId
            private$..cacheState$item$filePath <- filePath
            private$..cacheState$item$size <- size
            private$..cacheState$item$expired <- FALSE
            private$..cacheState$item$created <- Sys.time()
            private$..cacheState$item$modified <- Sys.time()
            private$..cacheState$item$accessed <- Sys.time()
            private$..cacheState$item$nAccessed <- 0

            if (nrow(subset(private$..cacheState$inventory, id == objectId)) == 0) {
              private$addCacheInventoryFromFile()
            } else {
              private$updateCacheInventoryFromFile()
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

            while(private$..cacheState$settings$currentSize  >
                  private$..cacheState$settings$maxSize) {
              if (private$..cacheState$settings$trimPolicy == "LRU") {
                oldest <- subset(private$..cacheState$inventory, accessed == min(accessed) & id != objectId & expired == FALSE)
                expire <- subset(oldest, size == max(size))

              } else {
                leastUsed <- subset(private$..cacheState$inventory, nAccessed == min(nAccessed) & id != objectId & expired == FALSE)
                expire <- subset(leastUsed, size == max(size))
                expire <- subset(leastUsed, accessed == min(accessed))
              }
              private$..cacheState$inventory[id == expire$id, size := 0]
              private$..cacheState$inventory[id == expire$id, expired := TRUE]
              private$..cacheState$inventory[id == expire$id, filePath := "NA"]
              private$..cacheState$inventory[id == expire$id, modified := Sys.time()]
              private$..cacheState$inventory[id == expire$id, accessed := Sys.time()]
              private$..cacheState$inventory[id == expire$id, nAccessed := 0]
              filePath <- expire$filePath
              file.remove(filePath)
              private$..cacheState$settings$currentSize <-
                private$..cacheState$settings$currentSize - expire$size
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
              }
              private$..cacheState$settings$maxSize <- value
              private$putCacheState()
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
                private$putCacheState()
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

          resetInventory = function() {
            private$getCacheState(reset = TRUE)
            return(private$..cacheState$inventory)
          },

          #-------------------------------------------------------------------------#
          #                             IO Methods                                  #
          #-------------------------------------------------------------------------#
          read = function(object, io = NULL) {

            private$..methodName <- 'read'

            # Confirm file exists
            filePath <- private$getFilePath(object)
            private$checkFileExists(filePath)

            # Read data
            if (is.null(io)) io <- IOFactory$new(filePath)$getIOStrategy()
            data <- io$read(path = filePath)

            # Update cache state
            private$logReadCache(object)

            # Log
            private$..state <- paste0("Read ", object$getName(), " from cache. ")
            self$logIt()

            return(data)
          },

          write = function(object, content, io = NULL) {

            private$..methodName <- 'write'

            # Get file path
            filePath <- private$getFilePath(object)

            # Write data
            if (is.null(io)) io <- IOFactory$new(filePath)$getIOStrategy()
            io$write(path = filePath, content = content)

            # Trim if cache is full
            private$trim(object)

            # Update cache state
            cache <- private$logWriteCache(object)

            # Log
            private$..state <- paste0("Saved ", object$getName(), " to cache. ")
            self$logIt()

            invisible(self)
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
            cache <- list(
              inventory = private$..inventory,
              maxSize = private$..maxSize,
              currentSize = private$..currentSize,
              trimPolicy = private$..trimPolicy,
              item = private$..item
            )
            return(cache)
          },

          #-------------------------------------------------------------------------#
          #                            Log Method                                   #
          #-------------------------------------------------------------------------#
          logIt = function(level = 'Info', fieldName = NA) {

            private$..logs$entry$className <- private$..className
            private$..logs$entry$methodName <- private$..methodName
            private$..logs$entry$level <- level
            private$..logs$entry$msg <- private$..state
            private$..logs$entry$fieldName <- fieldName
            private$..logs$created <- Sys.time()
            private$..logs$writeLog()
          }
        )
      )
      super$initialize(...)
    }
  )
)#$new()
