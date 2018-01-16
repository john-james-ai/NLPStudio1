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
    ..created = character(),
    ..modified = character(),
    ..accessed = character(),

    createId = function() {
      settings <- hashids::hashid_settings(salt = 'this is my salt', min_length = 5)
      hashid <- hashids::encode(as.integer(private$..created) * 1000, settings)
      id <- toupper(hashid)
      return(id)
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
    #                           Metadata Methods                              #
    #-------------------------------------------------------------------------#
    meta = function(key = NULL, value = NULL) {

      private$..methodName <- 'meta'

      # Return meta data
      if (is.null(key) & is.null(value)) {
        m <- Filter(Negate(is.null), private$..meta)
        return(as.data.frame(m))
      }

      private$..meta[[key]] <- value

      # Log it
      private$..state <- paste0("Added ", key, " with value: ",value,
                                " to metadata for ",private$..meta[["name"]],
                                ".")
      self$logIt()

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
