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
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdAddCommaSpace"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- AddCommaSpace$new(x)$execute()
      return(x)
    }
  )
)
