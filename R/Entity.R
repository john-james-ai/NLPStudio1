#==============================================================================#
#                               Entity                                         #
#==============================================================================#
#' Entity
#'
#' \code{Entity} Base class for all entity related classes
#'
#' This base class defines members and methods common across all entity related
#' classes, including Studio, Corpus, Set, and Document classes.
#'
#' @section Document methods:
#'  \itemize{
#'   \item{\code{desc()}}{Active binding getter/setter for object description.}
#'   \item{\code{getName()}}{Method for retrieving an object's name.}
#'   \item{\code{getClassName()}}{Method for retrieving an object's class name}
#'   \item{\code{getPath()}}{Method for retrieving an object's path.}
#'  }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Entity <- R6::R6Class(
  classname = "Entity",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..name = character(),
    ..admin = list(
      className = character(),
      methodName = character(),
      path = character(),
      locked = FALSE,
      state = character(),
      logs = character(),
      created = character(),
      modified = character(),
      accessed = character()
    )
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Basic Get  Methods                            #
    #-------------------------------------------------------------------------#
    getClassName = function() private$..admin$className,
    getName = function() private$..name,
    getPath = function() private$..admin$path,

    #-------------------------------------------------------------------------#
    #                            Log Method                                   #
    #-------------------------------------------------------------------------#
    logIt = function(level = 'Info', fieldName = NA) {

      private$..admin$logs$entry$className <- private$..admin$className
      private$..admin$logs$entry$methodName <- private$..admin$methodName
      private$..admin$logs$entry$level <- level
      private$..admin$logs$entry$msg <- private$..admin$state
      private$..admin$logs$entry$fieldName <- fieldName
      private$..admin$logs$created <- Sys.time()
      private$..admin$logs$writeLog()
    }
  )
)
