#------------------------------------------------------------------------------#
#                            AddCommaSpaceCmd                                  #
#------------------------------------------------------------------------------#
#' AddCommaSpaceCmd
#'
#' \code{AddCommaSpaceCmd} Command for the AddCommaSpace class.
#'
#' Class that encapsulates the command to execute an object of the AddCommaSpace
#' class
#'
#' @usage AddCommaSpaceCmd$new()
#'
#' @template textStudioParams
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
AddCommaSpaceCmd <- R6::R6Class(
  classname = "AddCommaSpaceCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "AddCommaSpaceCmd"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- AddCommaSpace$new(x)$execute()
      return(x)
    }
  )
)
