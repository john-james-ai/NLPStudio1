#------------------------------------------------------------------------------#
#                              Remove Punctuation                              #
#------------------------------------------------------------------------------#
#' RemovePunct
#'
#' \code{RemovePunct} Removes punctuation from text.
#'
#' @usage RemovePunct$new(x, endmark = FALSE, apostrophe = FALSE)$execute()
#'
#' @template textStudioParams
#' @param endmark Logical indicating whether to remove endmarks.
#' @param apostrophe Logical indicating whether to remove apostrophes.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
RemovePunct <- R6::R6Class(
  classname = "RemovePunct",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Text0,

  public = list(
    initialize = function(x, endmark = FALSE, apostrophe = FALSE) {
      private$..className <- "RemovePunct"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "RemovePunct"
      private$..x <- x

      if (endmark == FALSE & apostrophe == FALSE) {
        private$..regex <- "(?![.?!'])[[:punct:]]"
      } else if (endmark == FALSE) {
        private$..regex <- "(?![.?!])[[:punct:]]"
      } else if (apostrophe == FALSE) {
        private$..regex <- "(?!['])[[:punct:]]"
      } else {
        private$..regex <- "[[:punct:]]"
      }
      private$..replacement <- " "
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
