#------------------------------------------------------------------------------#
#                            Replace Terms                                     #
#------------------------------------------------------------------------------#
#' ReplaceTerms
#'
#' \code{ReplaceTerms}  Replace a vector of search terms
#'
#' A wrapper for \code{\link[textclean]{mgsub}} that takes a vector
#' of search terms and a vector or single value of replacements.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceTerms$new(x, pattern, replacement, leadspace = FALSE)$execute()
#'
#' @template textCleanParams
#' @param terms Character string(s) to be matched in the given character vector.
#' @param replacementment Character string equal in length to pattern or of length
#' one which are  a replacement for matched pattern.
#' @param leadspace logical.  If \code{TRUE} inserts a leading space in the
#' replacements.
#' @param trailspace logical.  If \code{TRUE} inserts a trailing space in the
#' replacements.
#' @param fixed logical. If \code{TRUE}, pattern is a string to be matched as is.
#' Overrides all conflicting arguments.
#' @param trim logical.  If \code{TRUE} leading and trailing white spaces are
#' removed and multiple white spaces are reduced to a single white space.
#' @param order.pattern logical.  If \code{TRUE} and \code{fixed = TRUE}, the
#' \code{pattern} string is sorted by number of characters to prevent substrings
#' replacing meta strings (e.g., \code{pattern = c("the", "then")} resorts to
#' search for "then" first).
#'
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @return \code{ReplaceTerms} - Returns a vector with the pattern replaced.
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceTerms <- R6::R6Class(
  classname = "ReplaceTerms",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(
    ..terms = character(),
    ..leadspace = logical(),
    ..trailspace = logical(),
    ..fixed = logical(),
    ..trim = logical(),
    ..orderPattern = character(),

    processText = function(content) {
      content <- textclean::mgsub(x = content,
                                  pattern = private$..terms,
                                  replacement = private$..replacement,
                                  leadspace = private$..leadspace,
                                  trailspace = private$..trailspace,
                                  fixed = private$..fixed,
                                  trim = private$..trim,
                                  order.pattern = private$..orderPattern)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, terms, replace, leadspace = FALSE,
                          trailspace = FALSE, fixed = TRUE, trim = FALSE,
                          orderPattern = fixed) {
      private$..className <- "ReplaceTerms"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceTerms"
      private$..x <- x
      private$..terms <- terms
      private$..replacement <- replacement
      private$..leadspace <- leadspace
      private$..trailspace <- trailspace
      private$..fixed <- fixed
      private$..trim <- trim
      private$..orderPattern <- orderPattern
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
