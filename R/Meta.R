#==============================================================================#
#                                  Meta                                        #
#==============================================================================#
#' Meta
#'
#' \code{Meta} Base class for all metadata related classes
#'
#' Defines the metadata object and methods which are applied to NLPStudio domain 
#' objects. This class includes the following types of metadata:
#' \itemize{
#'  \item object: user defined object metadata pertaining to a domain object.
#'  \item application: metadata used by the application such as create, modify, 
#'  and access dates.
#'  \item system: metadata used to capture system information such as the user, 
#'  operating system version and release associated with an object.
#'  }
#'  User can change object meta data. Application and system metadata are 
#'  managed by the application.
#'
#' @section Core methods:
#'  \itemize{
#'   \item{\code{new()}}{Creates an object of one of the Meta classes.}
#'   \item{\code{meta()}}{Adds or changes an element or elements (key / value pairs) 
#'   of a Meta object.
#'   }
#'  }
#'  
#'  @param name Character string representing the name of the object for which
#'   metadata is being created.
#'  @param cls Character string representing the class of  the object for which 
#'  metadata is being created. This should be obtained by using the class 
#'  function on the object in the calling environment.
#'  @param key Character string or vector representing the key(s) of the 
#'  key/value pair(s).
#'  @param value Character string or vector indicating the value(s) of the 
#'  key/value pair(s).
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Meta Classes
#' @export
Meta <- R6::R6Class(
  classname = "Meta",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..params = list(
      key = character(),
      value = character()
    ),
    ..meta = list(
      object = list(),
      app = list(),
      system = list()
    )
  ),

  public = list(
    initialize = function(name, cls) {
      private$..meta$object$name <- name
      private$..meta$app$class <- cls
      private$..meta$system$user <- Sys.info()[["user"]]
      private$..meta$system$machine <- Sys.info()[["machine"]]
      private$..meta$system$os <- Sys.info()[["sysname"]]
      private$..meta$system$release <- Sys.info()[["release"]]
      private$..meta$system$version <- Sys.info()[["version"]]
      private$..meta$dates$created <- Sys.time()
      private$..meta$dates$modified <- Sys.time()
      private$..meta$dates$accessed <- Sys.time()
      
      if (private$validateParams()$code == FALSE) stop()
      
      invisible(self)
    },
    
    getParams = function() {
      p <- list()
      p$key <- private$..key
      p$value <- private$..value
      return(p)
    },
    
    meta = function(key = NULL, value = NULL) {
      
      private$..key
      
      # Validate
      if (!is.null(key) & !is.null(value) & (length(key) != length(value))) {
        stop("Non-null key/value pairs must be of equal length")
      } else if (is.null(key) & (!(is.null(value)))) {
        stop(paste0("Unable to change meta data. No key provided for value."))
      }
      
      # Return all metadata
      if (is.null(key) & is.null(value)) {
        return(Filter(Negate(is.null), private$..meta$object))
        
      # Return selected metadata  
      } else if (!is.null(key) & is.null(value)) {
        return(private$..meta$object[[names(private$..meta$object) %in% key]])
      
      # Assign / add key value pairs  
      } else {
        for (i in 1:length(key)) {
          private$..meta$object[[key[i]]] <- value[i]
        }
      }
      invisible(self)
    },
    purge = function() {
      name <- private$..meta$object$name
      private$..meta$object <- list()
      private$..meta$object$name <- name
      invisible(self)
    },
    
    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$meta(self)
    }
  )
)
