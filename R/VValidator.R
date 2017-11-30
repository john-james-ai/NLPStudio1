#==============================================================================#
#                                   VValidator                                 #
#==============================================================================#
#' VValidator
#'
#'
#' \code{VValidator} Abstract class for the Validation Visitor Classes.
#' Defines a super-class with certain authorizations that are inherited
#' by sub-classes.
#'
#' \strong{VValidator Methods:}
#' The VValidator methods are as follows:
#'  \itemize{
#'   \item{\code{new(object)}}{Not implemented for this abstract class.}
#'
#' }
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VValidator <- R6::R6Class(
  classname = "VValidator",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Validator0,
  private = list(
    ..name = 'VValidator'
  ),

  public = list(
    initialize = function() stop("This method is not implemented for this abstract class.")
  )
)
