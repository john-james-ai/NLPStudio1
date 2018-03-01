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
#' @template textStudioParams
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
RemoveTwitterCmd <- R6::R6Class(
  classname = "RemoveTwitterCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

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
