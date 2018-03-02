#==============================================================================#
#                               Entity                                         #
#==============================================================================#
#' Entity
#'
#' \code{Entity} Base class for all entity related classes
#'
#' This base class defines members and methods common across all entity related
#' classes, including Studio, Corpus, Set, and Document classes.
#'
#' @section Document methods:
#'  \itemize{
#'   \item{\code{desc()}}{Active binding getter/setter for object description.}
#'   \item{\code{getName()}}{Method for retrieving an object's name.}
#'   \item{\code{getClassName()}}{Method for retrieving an object's class name}
#'   \item{\code{getPath()}}{Method for retrieving an object's path.}
#'  }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Entity <- R6::R6Class(
  classname = "Entity",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..id = character(),
    ..meta = list(),
    ..className = character(),
    ..methodName = character(),
    ..state = character(),
    ..logs = character(),

    createId = function() {
      settings <- hashids::hashid_settings(salt = 'this is my salt', min_length = 8)
      hashid <- hashids::encode(as.integer(private$..meta[['created']]) * 1000, settings)
      id <- toupper(hashid)
      return(id)
    },

    #-------------------------------------------------------------------------#
    #           Instantiation Parameter Validation Method                     #
    #-------------------------------------------------------------------------#
    validateParams = function() {

      private$..methodName <- "initialize"
      v <- Validator$new()
      status <- v$init(self)
      if (status$code == FALSE) {
        private$..state <- status$msg
        self$logIt("Error")
      }
      return(status)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Basic Get  Methods                            #
    #-------------------------------------------------------------------------#
    getClassName = function() private$..className,
    getName = function() private$..meta[["name"]],
    getId = function() private$..id,
    
    #-------------------------------------------------------------------------#
    #                             IO Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(path, io = NULL) {
      
      private$..methodName <- 'read'
      
      # Update text file metadata
      private$..meta[["filePath"]] <- path
      private$..meta[["fileName"]] <- basename(path)
      
      # Read content
      if (is.null(io))  io <- IOFactory$new(private$..meta[["filePath"]])$getIOStrategy()
      private$..content <- io$read(path = private$..meta[["filePath"]])
      private$..state <- paste0("Read Text id ", private$..id, " from ",
                                private$..meta[["filePath"]], ".")
      self$logIt()
      
      private$..meta[["user"]] <- Sys.info()[["user"]]
      private$..meta[["modified"]] <- file.info(path)[["mtime"]]
      private$..meta[["accessed"]] <- Sys.time()
      
      invisible(self)
    },
    
    write = function(path, io = NULL) {
      
      private$..methodName <- 'write'
      
      # Update text file metadata
      private$..meta[["filePath"]] <- path
      private$..meta[["fileName"]] <- basename(path)
      
      # Write text file
      if (is.null(io))  io <- IOFactory$new(private$..[["filePath"]])$getIOStrategy()
      io$write(path = private$..meta[["filePath"]], content = private$..content)
      
      private$..state <- paste0("Saved Text id ", private$..meta["id"], " to ", path, ". ")
      self$logIt()
      
      private$..meta[["user"]] <- Sys.info()
      private$..meta[["modified"]] <- file.info(path)[["mtime"]]
      private$..meta[["accessed"]] <- Sys.time()
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Metadata Methods                              #
    #-------------------------------------------------------------------------#
    meta = function(key = NULL, value = NULL, purge = FALSE) {

      private$..methodName <- 'meta'
      
      # The available metadata variables can be changed for Corpus and Document
      # objects only.
      if (!(c("Document", "Corpus") %in% class(self))) {
        private$..state <- paste0("The meta method is only implemented for the ", 
                                  "Document and Corpus classes.")
        self$logIt("Error")
        stop()
      }
      
      if (isTRUE(purge)) {
        private$..meta <- list()
        
      } else  if (is.null(key) & is.null(value)) {
        m <- Filter(Negate(is.null), private$..meta)
        return(as.data.frame(m, stringsAsFactors = FALSE))
        
      } else if (!is.null(key) & is.null(value)) {
        return(private$..meta[[key]])
        
      } else {
        private$..meta[[key]] <- value
        
        # Log it
        private$..state <- paste0("Added ", key, " with value: ",value,
                                  " to metadata for ",private$..meta[["name"]],
                                  ".")
        self$logIt()
      }
      invisible(self)
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
