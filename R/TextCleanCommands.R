#==============================================================================#
#                           TextClean Command Classes                          #
#==============================================================================#

#------------------------------------------------------------------------------#
#                                 CmdText0                                     #
#------------------------------------------------------------------------------#
#' CmdText0
#'
#' \code{CmdText0} Abstract class  for the TextClean family of classes.
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
CmdText0 <- R6::R6Class(
  classname = "CmdText0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..x = character()
  ),

  public = list(
    initialize = function() { stop("Not implemented for this abstract/interface class.") },
    execute = function(x) { stop("Not implemented for this abstract/interface class.") }
  )
)

#------------------------------------------------------------------------------#
#                            CmdAddCommaSpace                                  #
#------------------------------------------------------------------------------#
#' CmdAddCommaSpace
#'
#' \code{CmdAddCommaSpace} Command for the AddCommaSpace class.
#'
#' Class that encapsulates the command to execute an object of the AddCommaSpace
#' class
#'
#' @usage CmdAddCommaSpace$new()
#'
#' @template textCleanParams
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
CmdAddCommaSpace <- R6::R6Class(
  classname = "CmdAddCommaSpace",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  public = list(
    initialize = function() {
      private$..meta[["name"]] <- "CmdAddCommaSpace"
      invisible(self)
    },
    execute = function(x) {
      x <- AddCommaSpace$new(x)$execute()
      return(x)
    }
  )
)
