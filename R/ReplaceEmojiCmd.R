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
#' @template textStudioParams
#' @param emojis A data.table of emojis (ASCII byte representations) and
#' corresponding word/identifier meanings.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceEmojiCmd <- R6::R6Class(
  classname = "ReplaceEmojiCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  
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
