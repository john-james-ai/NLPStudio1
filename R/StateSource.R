#==============================================================================#
#                                 StateSource                                       #
#==============================================================================#
#' StateSource
#'
#' \code{StateSource} The 'State' abstract class
#'
#' @section StateSource Core Methods:
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
#' @param name A character string containing the name of the StateSource object. This variable is used in the instantiation and remove methods.
#' @param object An object associated with this stage in the pipeline.
#' @param path A character string containing the relative file path to this stage.
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Stage classes
#' @export
StateSource <- R6::R6Class(
  classname = "StateSource",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = State0,

  public = list(

    #-------------------------------------------------------------------------#
    #                         StateSource Core Methods                        #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$..admin$className <- 'StateSource'
      private$..admin$methodName <- 'initialize'
      private$..admin$state <- "Instantiated StateSource object."
      private$..admin$logs <- LogR$new()
      private$..admin$modified <- Sys.time()
      private$..admin$created <- Sys.time()
      private$..admin$accessed <- Sys.time()
      self$logIt()
      invisible(self)
    },
    process = function(dataSource) {
      fc <- dataSource$execute()
      self$addObject(fc)
      private$..admin$state <- paste0("Data obtained from source")
      self$logIt()
      private$..pipeline$state <- StateRepair$new(self)
      return(fc)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$stateInit(self)
    }
  )
)
