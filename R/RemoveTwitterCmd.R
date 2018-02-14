#------------------------------------------------------------------------------#
#                            RemoveTwitterCmd                                  #
#------------------------------------------------------------------------------#
#' RemoveTwitterCmd
#'
#' \code{RemoveTwitterCmd} Command for the RemoveTwitter class.
#'
#' Class that encapsulates the command to execute an object of the RemoveTwitter
#' class
#'
#' @usage RemoveTwitterCmd$new()
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
RemoveTwitterCmd <- R6::R6Class(
  classname = "RemoveTwitterCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCmd0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "RemoveTwitterCmd"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveTwitter$new(x)$execute()
      return(x)
    }
  )
)
