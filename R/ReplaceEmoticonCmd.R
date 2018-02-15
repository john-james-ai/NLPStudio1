#------------------------------------------------------------------------------#
#                           ReplaceEmoticonCmd                                 #
#------------------------------------------------------------------------------#
#' ReplaceEmoticonCmd
#'
#' \code{ReplaceEmoticonCmd} Command for the ReplaceEmoticon class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceEmoticon
#' class
#'
#' @usage ReplaceEmoticonCmd$new(emoticons = NULL)
#'
#' @template textStudioParams
#' @param emoticons A data.table of emoticons (graphical representations) and
#' corresponding word meanings.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceEmoticonCmd <- R6::R6Class(
  classname = "ReplaceEmoticonCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  

  private = list(
    ..emoticons = data.table()
  ),

  public = list(
    initialize = function(emoticons = NULL) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "ReplaceEmoticonCmd"
      private$..emoticons <- emoticons
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceEmoticon$new(x, private$..emoticons)$execute()
      return(x)
    }
  )
)
