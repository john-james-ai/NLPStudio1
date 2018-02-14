#------------------------------------------------------------------------------#
#                            RemoveEmailCmd                                    #
#------------------------------------------------------------------------------#
#' RemoveEmailCmd
#'
#' \code{RemoveEmailCmd} Command for the RemoveEmail class.
#'
#' Class that encapsulates the command to execute an object of the RemoveEmail
#' class
#'
#' @usage RemoveEmailCmd$new()
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
RemoveEmailCmd <- R6::R6Class(
  classname = "RemoveEmailCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCmd0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "RemoveEmailCmd"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveEmail$new(x)$execute()
      return(x)
    }
  )
)