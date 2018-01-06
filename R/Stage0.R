#==============================================================================#
#                                 Stage0                                       #
#==============================================================================#
#' Stage0
#'
#' \code{Stage0} Class for defining the pipeline stages and their data objects
#'
#' @section Stage0 Core Methods:
#'  \describe{
#'   \item{\code{new(name, path)}}{Creates an object of Stage0 Class}
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
#' @param name A character string containing the name of the Stage0 object. This variable is used in the instantiation and remove methods.
#' @param object An object associated with this stage in the pipeline.
#' @param path A character string containing the relative file path to this stage.
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Stage classes
#' @export
Stage0 <- R6::R6Class(
  classname = "Stage0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..objects = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Stage0 Instantiation                             #
    #-------------------------------------------------------------------------#
    initialize = function(name, path) { stop("This method is not implemented for this abstract class.") },

    #-------------------------------------------------------------------------#
    #                     Object Aggregation Methods                          #
    #-------------------------------------------------------------------------#
    getObjects = function()  private$..objects,

    addObject = function(object) {

      private$..admin$methodName <- 'addObject'
      name <- object$getName()
      private$..objects[[name]] <- object
      private$..admin$state <- paste0("Added ", name, " to ", private$..name)
      self$logIt()
      invisible(self)

    },

    removeObject = function(object) {

      private$..admin$methodName <- 'removeObject'
      name <- getName(object)
      private$..objects[[name]] <- NULL
      private$..admin$state <- paste0("Removed ", name, " from ", private$..name)
      self$logIt()
      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  { stop("This method is not implemented for this abstract class.") }
  )
)
