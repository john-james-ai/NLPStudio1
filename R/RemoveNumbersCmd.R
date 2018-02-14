#------------------------------------------------------------------------------#
#                            RemoveNumbersCmd                                  #
#------------------------------------------------------------------------------#
#' RemoveNumbersCmd
#'
#' \code{RemoveNumbersCmd} Command for the RemoveNumbers class.
#'
#' Class that encapsulates the command to execute an object of the RemoveNumbers
#' class
#'
#' @usage RemoveNumbersCmd$new()
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
RemoveNumbersCmd <- R6::R6Class(
  classname = "RemoveNumbersCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCmd0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "RemoveNumbersCmd"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveNumbers$new(x)$execute()
      return(x)
    }
  )
)
