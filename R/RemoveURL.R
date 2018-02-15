#------------------------------------------------------------------------------#
#                              Remove URL                                      #
#------------------------------------------------------------------------------#
#' RemoveURL
#'
#' \code{RemoveURL} Removes URLs.
#'
#' Removes URLs  from text.
#'
#' @usage RemoveURL$new(x)$execute()
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
RemoveURL <- R6::R6Class(
  classname = "RemoveURL",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Text0,

  public = list(
    initialize = function(x) {
      private$..className <- "RemoveURL"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemoveURL"
      private$..x <- x
      private$..regex <- "(?:(?:https?:\\/\\/)|(?:www\\.))[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,4}\\b(?:[-a-zA-Z0-9@:%_\\+.~#?&/=]*)"

      private$..replacement <- " "
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
