#------------------------------------------------------------------------------#
#                              Replace Backtick                                #
#------------------------------------------------------------------------------#
#' ReplaceBacktick
#'
#' \code{ReplaceBacktick} Replace backtick
#'
#' Replaces backticks with single quotes.
#'
#' @usage ReplaceBacktick$new(x)$execute()
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
ReplaceBacktick <- R6::R6Class(
  classname = "ReplaceBacktick",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  public = list(
    initialize = function(x) {
      private$..className <- "ReplaceBacktick"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceBacktick"
      private$..x <- x
      private$..regex <- "\`"

      private$..replacement <- "'"
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
