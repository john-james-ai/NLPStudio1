#------------------------------------------------------------------------------#
#                         Replace Emoji                                        #
#------------------------------------------------------------------------------#
#' ReplaceEmoji
#'
#' \code{ReplaceEmoji}  Replace emojis with the words they represent.
#'
#' A wrapper for \code{\link[textclean]{replace_emoji}} that replaces
#' emojis with the words they represent.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceEmoji$new(x, emojis = NULL)$execute()
#'
#' @template textCleanParams
#' @param emojis A data.table of emojis (ASCII byte representations) and
#' corresponding word/identifier meanings.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceEmoji} Returns a vector with emojis replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceEmoji <- R6::R6Class(
  classname = "ReplaceEmoji",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Text0,

  private = list(
    ..emojis = data.table(),

    processText = function(content) {
      if (is.null(private$..emojis)) {
        content <- textclean::replace_emoji(x = content)
      } else {
        content <- textclean::replace_emoji(x = content,
                                            emoji_dt = private$..emojis)
      }
      return(content)
    }
  ),

  public = list(
    initialize = function(x, emojis = NULL) {
      private$..className <- "ReplaceEmoji"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceEmoji"
      private$..x <- x
      private$..emojis <- emojis
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
