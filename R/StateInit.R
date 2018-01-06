#==============================================================================#
#                                 StateInit                                       #
#==============================================================================#
#' StateInit
#'
#' \code{StateInit} The 'State' abstract class
#'
#' @section StateInit Core Methods:
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
#' @param name A character string containing the name of the StateInit object. This variable is used in the instantiation and remove methods.
#' @param object An object associated with this stage in the pipeline.
#' @param path A character string containing the relative file path to this stage.
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Stage classes
#' @export
StateInit <- R6::R6Class(
  classname = "StateInit",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = State0,

  private = list(
    ..path = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         StateInit Core Methods                          #
    #-------------------------------------------------------------------------#
    initialize = function(name, path) {
      private$..admin$className <- 'StateInit'
      private$..admin$methodName <- 'initialize'
      private$..path <- path
      private$..admin$state <- "Instantiated StateInit object"



      invisible(self)
    },
    process = function(path) {

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$stateInit(self)
    }
  )
)
