#==============================================================================#
#                                 State0                                       #
#==============================================================================#
#' State0
#'
#' \code{State0} The 'State' abstract class
#'
#' @section State0 Core Methods:
#'  \describe{
#'   \item{\code{new()}}{Instantiates a State class object.}
#'   \item{\code{process()}}{Method for implementing state-specific behavior.}
#'  }
#'
#' @section Other Methods:
#'  \describe{
#'   \item{\code{logIt(level = 'Info', fieldName = NA)}}{Formats the log and calls the LogR class to log an event.}
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#'  }
#'
#' @param name A character string containing the name of the State0 object. This variable is used in the instantiation and remove methods.
#' @param object An object associated with this stage in the pipeline.
#' @param path A character string containing the relative file path to this stage.
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Stage classes
#' @export
State0 <- R6::R6Class(
  classname = "State0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..pipeline = character(),
    ..objects = list()
  ),

  active = list(
    pipeline = function(value) {
      if (missing(value)) {
        private$..pipeline
      } else {
        if ('Pipeline' %in% class(value)) {
          private$..pipeline <- value
        } else {
          private$..state <- "Invalid Pipeline object"
          self$logIt('Error')
          stop()
        }
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         State0 Core Methods                             #
    #-------------------------------------------------------------------------#
    initialize = function() { stop("This method is not implemented for this abstract class.") },
    process = function() { stop("This method is not implemented for this abstract class.") },

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
