#==============================================================================#
#                               StageDataPrep                                  #
#==============================================================================#
#' StageDataPrep
#'
#' \code{StageDataPrep} Class responsible for creating and executing the data preparation stage of the pipeline.
#'
#' @section StageDataPrep Core Methods:
#'  \describe{
#'   \item{\code{new(name, path)}}{Creates an object of StageDataPrep Class}
#'   \item{\code{getObjects()}}{Retrieves the objects associated with this stage.}
#'   \item{\code{addObject(object)}}{Adds an object to this stage. }
#'   \item{\code{removeObject(object)}}{Removes an object from this stage. }
#'  }
#'
#' @section Other Methods:
#'  \describe{
#'   \item{\code{logIt(level = 'Info', fieldName = NA)}}{Formats the log and calls the LogR class to log an event.}
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#'  }
#'
#' @param name A character string containing the name of the StageDataPrep object. This variable is used in the instantiation and remove methods.
#' @param object An object associated with this stage in the pipeline.
#' @param path A character string containing the relative file path to this stage.
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Stage classes
#' @export
StageDataPrep <- R6::R6Class(
  classname = "StageDataPrep",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Stage0,

  private = list(
    ..objects = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         StageDataPrep Instantiation                     #
    #-------------------------------------------------------------------------#
    initialize = function(name, path) {

      # Instantiate variables
      private$..name <- name
      private$..path <- path
      private$..admin$className <- 'StageDataPrep'
      private$..admin$methodName <- 'initialize'
      private$..admin$state <- paste0("StageDataPrep, ", name, ", instantiated.")
      private$..admin$modified <- Sys.time()
      private$..admin$created <- Sys.time()
      private$..admin$accessed <- Sys.time()
      private$..admin$logs <- LogR$new()

      # Validate
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..admin$state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Create directory
      dir.create(private$..path, showWarnings = FALSE, recursive = TRUE)

      # Create log entry
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$stageDataPrep(self)
    },

    #-------------------------------------------------------------------------#
    #                           Test Methods                                  #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      #TODO: Remove after testing

      stage = list(
        name = private$..name,
        path = private$..path,
        objects = private$..objects,
        logs = private$..admin$logs,
        state = private$..admin$state,
        modified = private$..admin$modified,
        created = private$..admin$created,
        accessed = private$..admin$accessed
      )

      return(stage)
    }
  )
)
