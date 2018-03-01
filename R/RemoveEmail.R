#------------------------------------------------------------------------------#
#                              Remove Email Addresses                          #
#------------------------------------------------------------------------------#
#' RemoveEmail
#'
#' \code{RemoveEmail} Removes email addresses from text.
#'
#' @usage RemoveEmail$new(x)$execute()
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
RemoveEmail <- R6::R6Class(
  classname = "RemoveEmail",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function(x) {
      private$..className <- "RemoveEmail"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveEmail"
      private$..x <- x
      private$..regex <- "[a-zA-Z0-9\\-_~]+(\\.[a-zA-Z0-9\\-_~]+)*@[a-zA-Z0-9\\-_~]+(\\.[a-zA-Z0-9\\-_~]+)*\\.[a-zA-Z]{2,}"
      private$..replacement <- ""
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
