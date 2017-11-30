#==============================================================================#
#                                 LogTest                                      #
#==============================================================================#
#' LogTest
#'
#' \code{LogTest} Logs test information
#'
#' Logs test information
#'
#' @section Log test methods:
#'
#' \strong{LogTest Methods:}
#'  \itemize{
#'   \item{\code{initialize()}}{Method for initializing test logger.}
#'   \item{\code{writeLog()}}{Method for writing to the log.}
#' }
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
LogTest <- R6::R6Class(
  classname = "LogTest",
  lock_objects = TRUE,
  lock_class = TRUE,

  private = list(
    ..path = "./test/logs",
    ..logs = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$..logs <- Logger$new(private$..path)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Log  Method                                 #
    #--------------------------------------------------------,----------------#
    logs = function(level = 'Info', className, methodName, msg, fieldName = NA) {

      # Create logger and initialization log entry
      private$..logs$entry$owner <- match.call()[1]
      private$..logs$entry$className <- className
      private$..logs$entry$methodName <- methodName
      private$..logs$entry$path <- private$..path
      private$..logs$entry$level <- level
      private$..logs$entry$msg <- msg
      private$..logs$entry$fieldName <- fieldName
      private$..logs$entry$created <- Sys.time()
      private$..logs$writeLog()

    }
  )
)
