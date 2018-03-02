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
#' @section Entity methods:
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
    ..meta = character(),
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
    getName = function() private$..meta$object[["name"]],
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
