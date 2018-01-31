#==============================================================================#
#                                 Preprocess0                                     #
#==============================================================================#
#' Preprocess0
#'
#' \code{Preprocess0} Abstract superclass that defines the methods common to all Preprocess family classes.
#'
#' This abstract superclass defines the methods and interfaces common to all Preprocess family classes. It inherits from the Entity class.
#'
#' @template preprocessClasses.R
#' @template preprocessMethods.R
#' @template preprocessParams.R
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family PrePreprocess Family of Classes
#' @export
Preprocess0 <- R6::R6Class(
  classname = "Preprocess0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..in = character(),
    ..out = character(),
    ..substitutions = data.frame()
  ),

  public = list(

    initialize = function(object, name, ...) { stop("This method is not implemented for this abstract class") },
    getInObject = function() private$..in,
    Preprocess = function() { stop("This method is not implemented for this abstract class") },
    #-------------------------------------------------------------------------#
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$Preprocess0(self)
    }
  )
)
