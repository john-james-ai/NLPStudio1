#------------------------------------------------------------------------------#
#                                 TextCmd0                                     #
#------------------------------------------------------------------------------#
#' TextCmd0
#'
#' \code{TextCmd0} Abstract class  for the TextClean family of classes.
#'
#' This abstract class defines a common interface and methods for the TextClean
#' family of classes.
#'
#' @template textCleanParams
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean classes
#' @export
TextCmd0 <- R6::R6Class(
  classname = "TextCmd0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..x = character(),
    ..replacement = character()
  ),

  public = list(
    initialize = function() { stop("Not implemented for this abstract/interface class.") },
    execute = function(x) { stop("Not implemented for this abstract/interface class.") }
  )
)
