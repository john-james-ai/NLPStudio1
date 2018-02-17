#==============================================================================#
#                           Split0                                #
#==============================================================================#
#' Split0
#'
#' \code{Split0} Abstract superclass that defines the methods common to all Preprocess family classes.
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
Split0 <- R6::R6Class(
  classname = "Split0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Text0,

  private = list(
    ..x = character(),
    ..name = character(),
    ..trainSize = 0,
    ..valSize = 0,
    ..testSize = 0,
    ..seed = numeric(),
    ..cvSet = list()
  ),

  public = list(

    initialize = function(x, ...) { stop("This method is not implemented for this abstract class") },
    execute = function() { stop("This method is not implemented for this abstract class") },
    getParams = function() {
      params <- list(
        x = private$..x,
        trainSize = private$..trainSize,
        valSize = private$..valSize,
        testSize = private$..testSize,
        seed = private$..seed,
        cvSet = private$..cvSet
      )
      return(params)
    }
  )
)
