#------------------------------------------------------------------------------#
#                               CmdReplaceTerms                                #
#------------------------------------------------------------------------------#
#' CmdReplaceTerms
#'
#' \code{CmdReplaceTerms} Command for the ReplaceTerms class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceTerms
#' class
#'
#' @usage CmdReplaceTerms$new(joinTerms = FALSE, remove = FALSE)
#'
#' @template textCleanParams
#' @param terms Character string(s) to be matched in the given character vector.
#' @param replacement Character string equal in length to pattern or of length
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
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
CmdReplaceTerms <- R6::R6Class(
  classname = "CmdReplaceTerms",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  private = list(
    ..terms = character(),
    ..leadspace = logical(),
    ..trailspace = logical(),
    ..fixed = logical(),
    ..trim = logical(),
    ..orderPattern = character()
  ),

  public = list(
    initialize = function(terms, replace, leadspace = FALSE, trailspace = FALSE,
                          fixed = TRUE, trim = FALSE, orderPattern = fixed) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdReplaceTerms"
      private$..terms <- terms
      private$..replace <- replace
      private$..leadspace <- leadspace
      private$..trailspace <- trailspace
      private$..fixed <- fixed
      private$..trim <- trim
      private$..orderPattern <- orderPattern
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceTerms$new(x, terms = private$..terms,
                            replace = private$..replace,
                            leadspace = private$..leadspace,
                            trailspace = private$..trailspace,
                            fixed = private$..fixed,
                            trim = private$..trim,
                            orderPattern = private$..orderPattern)$execute()
      return(x)
    }
  )
)
