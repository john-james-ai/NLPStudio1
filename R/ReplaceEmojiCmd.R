#------------------------------------------------------------------------------#
#                           ReplaceEmojiCmd                                    #
#------------------------------------------------------------------------------#
#' ReplaceEmojiCmd
#'
#' \code{ReplaceEmojiCmd} Command for the ReplaceEmoji class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceEmoji
#' class
#'
#' @usage ReplaceEmojiCmd$new(emojis = NULL)
#'
#' @template textCleanParams
#' @param emojis A data.table of emojis (ASCII byte representations) and
#' corresponding word/identifier meanings.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceEmojiCmd <- R6::R6Class(
  classname = "ReplaceEmojiCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCmd0,

  private = list(
    ..emojis = data.table()
  ),

  public = list(
    initialize = function(emojis = NULL) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "ReplaceEmojiCmd"
      private$..emojis <- emojis
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceEmoji$new(x, private$..emojis)$execute()
      return(x)
    }
  )
)
