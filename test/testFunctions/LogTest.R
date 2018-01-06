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
    ..admin = list(
      path = "./test/logs",
      logs = character()
    )
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function() {
      private$..admin$logs <- LogR$new(private$..path)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Log  Method                                 #
    #--------------------------------------------------------,----------------#
    logs = function(level = 'Info', className, methodName, msg, fieldName = NA) {

      # Create logger and initialization log entry
      private$..admin$logs$entry$owner <- match.call()[1]
      private$..admin$logs$entry$className <- className
      private$..admin$logs$entry$methodName <- methodName
      private$..admin$logs$entry$path <- private$..path
      private$..admin$logs$entry$level <- level
      private$..admin$logs$entry$msg <- msg
      private$..admin$logs$entry$fieldName <- fieldName
      private$..admin$logs$entry$created <- Sys.time()
      private$..admin$logs$writeLog()

    }
  )
)
