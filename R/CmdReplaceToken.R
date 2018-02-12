#------------------------------------------------------------------------------#
#                               CmdReplaceToken                                #
#------------------------------------------------------------------------------#
#' CmdReplaceToken
#'
#' \code{CmdReplaceToken} Command for the ReplaceToken class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceToken
#' class
#'
#' @usage CmdReplaceToken$new(tokens,  replace = NULL, ignoreCase = TRUE)
#'
#' @template textCleanParams
#' @param tokens A vector of token to be replaced.
#' @param replace A single character string to replace the tokens with. The default, NULL, replaces the tokens with nothing.
#' @param ignoreCase logical. If TRUE the case of the tokens will be ignored.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
CmdReplaceToken <- R6::R6Class(
  classname = "CmdReplaceToken",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  private = list(
    ..tokens = character(),
    ..ignoreCase = logical()
  ),

  public = list(
    initialize = function(tokens, replace = NULL, ignoreCase = TRUE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdReplaceToken"
      private$..tokens <- tokens
      private$..replace <- replace
      private$..ignoreCase <- ignoreCase
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceToken$new(x, tokens = private$..tokens,
                            replace = private$..replace,
                            ignoreCase = private$..ignoreCase)$execute()
      return(x)
    }
  )
)
