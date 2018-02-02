#==============================================================================#
#                           PreprocessStrategy0                                #
#==============================================================================#
#' PreprocessStrategy0
#'
#' \code{PreprocessStrategy0} Abstract superclass that defines the methods common to all Preprocess family classes.
#'
#' This abstract superclass defines the methods and interfaces common to all Preprocess family classes. It inherits from the Entity class.
#'
#' @template preprocessClasses
#' @template preprocessMethods
#' @template preprocessParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Preprocess Family of Classes
#' @export
PreprocessStrategy0 <- R6::R6Class(
  classname = "PreprocessStrategy0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..in = character(),
    ..out = character(),
    ..substitutions = NULL,
    ..trainSize = 0,
    ..valSize = 0,
    ..testSize = 0,
    ..seed = numeric(),
    ..cvSet = list()
  ),

  public = list(

    initialize = function(object, ...) { stop("This method is not implemented for this abstract class") },
    getInObject = function() private$..in,
    Preprocess = function() { stop("This method is not implemented for this abstract class") },
    #-------------------------------------------------------------------------#
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$PreprocessStrategy0(self)
    }
  )
)
