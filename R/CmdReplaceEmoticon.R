#------------------------------------------------------------------------------#
#                           CmdReplaceEmoticon                                 #
#------------------------------------------------------------------------------#
#' CmdReplaceEmoticon
#'
#' \code{CmdReplaceEmoticon} Command for the ReplaceEmoticon class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceEmoticon
#' class
#'
#' @usage CmdReplaceEmoticon$new(emoticons = NULL)
#'
#' @template textCleanParams
#' @param emoticons A data.table of emoticons (graphical representations) and
#' corresponding word meanings.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
CmdReplaceEmoticon <- R6::R6Class(
  classname = "CmdReplaceEmoticon",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  private = list(
    ..emoticons = data.table()
  ),

  public = list(
    initialize = function(emoticons = NULL) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdReplaceEmoticon"
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
