#------------------------------------------------------------------------------#
#                             Replace Token                                    #
#------------------------------------------------------------------------------#
#' ReplaceToken
#'
#' \code{ReplaceToken}  Replace Tokens.
#'
#' A wrapper for \code{\link[textclean]{replace_tokens}} Replace tokens with a single substring.
#' This is much faster than mgsub if one wants to replace fixed tokens with a
#' single value or remove them all together. This can be useful for quickly
#' replacing tokens like names in string with a single value in order to reduce noise.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceToken$new(x, token, replace, ignoreCase = TRUE)$execute()
#'
#' @template textCleanParams
#' @param tokens A vector of token to be replaced.
#' @param replacement A single character string to replace the tokens with. The default, NULL, replaces the tokens with nothing.
#' @param ignoreCase logical. If TRUE the case of the tokens will be ignored.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceToken} Returns a vector with tokens replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceToken <- R6::R6Class(
  classname = "ReplaceToken",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(

    ..tokens = character(),
    ..ignoreCase = logical(),

    processText = function(content) {
      content <- textclean::replace_symbol(x = content,
                                           tokens = private$..tokens,
                                           replacement = private$..replacement,
                                           ignore.case = private$..ignoreCase)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, tokens, replacement = NULL, ignoreCase = TRUE) {
      private$..className <- "ReplaceToken"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceToken"
      private$..x <- x
      private$..tokens <- tokens
      private$..replacement <- replacement
      private$..ignoreCase <- ignoreCase
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
