#==============================================================================#
#                                 Validator                                    #
#==============================================================================#
#' Validator
#'
#' @description
#' \code{Validator} Class responsible for validation of requests pertaining to:
#' \itemize{
#'  \item Object instantiation
#'  \item Composition and Aggregation: Requests to manipulate aggregate and composite objects
#'  \item History: Requests to read historical records for an object or objects.#'
#'  \item State: Requests to read, save, and restore object states
#'  \item Document I/O: Requests to read and write documents.
#' }
#'
#' @section Validator methods:
#' This section summarizes the methods in the Validator class.
#'
#' \strong{Object Instantiation Methods::}
#' \describe{
#'  \item{\code{new()}}{Creates an object of Validator class}
#'  \item{\code{init(object)}}{Dispatches the initialization validation visitor, via the accept method of object.}
#'  }
#'
#' \strong{Composition and Aggregation Methods:}
#' \describe{
#'  \item{\code{addChild(object)}}{Dispatches the addChild validation visitor, via the accept method of object.}
#'  \item{\code{removeChild(object)}}{Dispatches the removeChild validation visitor, via the accept method of object.}
#'  \item{\code{setParent(object, parent)}}{Dispatches the setParent validation visitor, via the accept method of object.}
#'  }
#'
#' \strong{Document I/O Methods:}
#' \describe{
#'  \item{\code{read(object)}}{Dispatches the read validation visitor, via the accept method of object.}
#'  \item{\code{write(object)}}{Dispatches the write validation visitor, via the accept method of object.}
#' }
#'
#' @section Validation Class Parameters
#' @param object Originator object
#' @param target Object which is the target of a query. Used by the Curator methods
#' @param content Character vector containing content to be written by a write originator.
#'
#' @return A logical, TRUE if validation criteria are met, FALSE, otherwise.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family Validation Classes
#' @export
Validator <- R6::R6Class(
  "Validator",
  lock_class = FALSE,
  lock_objects = FALSE,

  public = list(

    #-------------------------------------------------------------------------#
    #                     Object Initiation and Creation                      #
    #-------------------------------------------------------------------------#
    init = function(object) {
      visitor <- VValidatorInit$new()
      object$accept(visitor)
    },

    build = function(object) {
      visitor <- VValidatorBuild$new()
      object$accept(visitor)
    },

    #-------------------------------------------------------------------------#
    #                   Composition and Aggregation                           #
    #-------------------------------------------------------------------------#
    addChild = function(object, child) {
      visitor <- VValidatorAddChild$new(object, child)
      object$accept(visitor)
    },
    removeChild = function(object, child) {
      visitor <- VValidatorRemoveChild$new(object, child)
      object$accept(visitor)
    },
    setParent = function(object, parent) {
      visitor <- VValidatorSetParent$new(object, parent)
      object$accept(visitor)
    },

    #-------------------------------------------------------------------------#
    #                            Administration                               #
    #-------------------------------------------------------------------------#
    meta = function(object, request) {
      visitor <- VValidatorDocument0$new(object, request)
      object$accept(visitor)
    }

  )
)
