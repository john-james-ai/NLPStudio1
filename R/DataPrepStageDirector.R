#==============================================================================#
#                             DataDirector                                     #
#==============================================================================#
#' DataDirector
#'
#' \code{DataDirector} Invoker class responsible for building the data sets for the pipeline through the DataBuilder class.
#'
#' Class which maintains a list of commands to be invoked via the DataBuilder class.
#'
#' @section DataDirector Methods:
#'  \describe{
#'   \item{\code{new()}}{Instantiates a DataDirector object.}
#'   \item{\code{construct()}}{Returns the cross validation sets. }
#'  }
#'
#' @section Parameters:
#' @param builder DataBuilder object
#' @param command Object of one of the Command classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
DataDirector <- R6::R6Class(
  classname = "DataDirector",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..commands = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         DataDirector Core Methods                       #
    #-------------------------------------------------------------------------#
    initialize = function() {

      private$..admin$className <- 'DataDirector'
      private$..admin$methodName <- 'initialize'
      private$..admin$state <- paste("DataDirector object instantiated.")
      private$..admin$logs <- LogR$new()

      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            Add Command Method                           #
    #-------------------------------------------------------------------------#
    addCommand = function(command) {
      private$..commands <- c(private$..commands, command)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            Execute Commands                             #
    #-------------------------------------------------------------------------#
    execute = function() {
      data <- lapply(seq_along(private$..commands), function(cmd) {
        private$..commands[[cmd]]$execute()
      })
      return(data)
    }
  )
)
