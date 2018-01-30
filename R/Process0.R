#==============================================================================#
#                                 Process0                                     #
#==============================================================================#
#' Process0
#'
#' \code{Process0} Abstract superclass that defines the methods common to all Process family classes.
#'
#' This abstract superclass defines the methods and interfaces common to all Process family classes. It inherits from the Entity class.
#'
#' @template processClasses.R
#' @template processMethods.R
#' @template processParams.R
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Process Family of Classes
#' @export
Process0 <- R6::R6Class(
  classname = "Process0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..in = character(),
    ..out = character()
  ),

  public = list(

    initialize = function(object, name, ...) { stop("This method is not implemented for this abstract class") },
    getInObject = function() private$..in,
    process = function() { stop("This method is not implemented for this abstract class") },
    #-------------------------------------------------------------------------#
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$process0(self)
    }
  )
)
