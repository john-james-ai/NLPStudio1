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
    ..log = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$..log <- Logger$new(private$..path)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Log  Method                                 #
    #--------------------------------------------------------,----------------#
    log = function(level = 'Info', className, methodName, msg, fieldName = NULL) {

      # Create logger and initialization log entry
      private$..log$entry$owner <- match.call()[1]
      private$..log$entry$className <- className
      private$..log$entry$methodName <- methodName
      private$..log$entry$path <- private$..path
      private$..log$entry$level <- level
      private$..log$entry$msg <- msg
      private$..log$entry$fieldName <- fieldName
      private$..log$entry$created <- Sys.time()
      private$..log$writeLog()

    }
  )
)
