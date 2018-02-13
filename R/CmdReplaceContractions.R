#------------------------------------------------------------------------------#
#                           CmdReplaceContractions                             #
#------------------------------------------------------------------------------#
#' CmdReplaceContractions
#'
#' \code{CmdReplaceContractions} Command for the ReplaceContractions class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceContractions
#' class
#'
#' @usage CmdReplaceContractions$new(contractions = NULL, ignoreCase = TRUE)
#'
#' @template textCleanParams
#' @param pattern Character string of contractions to be matched in the 
#' given character vector. If NULL, the default is to use \code{\link[lexicon]{key_contractions}} data set.
#' @param replacement Character string equal in length to pattern containing
#'  the long forms of the contractions. 
#' @param leadspace logical.  If \code{TRUE} inserts a leading space in the 
#' replacements.
#' @param trailspace logical.  If \code{TRUE} inserts a trailing space in the 
#' replacements.
#' @param fixed logical. If \code{TRUE}, pattern is a string to be matched as is. 
#' Overrides all conflicting arguments.
#' @param trim logical.  If \code{TRUE} leading and trailing white spaces are 
#' removed and multiple white spaces are reduced to a single white space.
#' @param orderPattern logical.  If \code{TRUE} and \code{fixed = TRUE}, the 
#' \code{pattern} string is sorted by number of characters to prevent substrings 
#' replacing meta strings (e.g., \code{pattern = c("the", "then")} resorts to 
#' search for "then" first).
#' @param \dots ignored.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
CmdReplaceContractions <- R6::R6Class(
  classname = "CmdReplaceContractions",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,
  
  private = list(
    ..pattern = character(),
    ..leadspace = logical(),
    ..trailspace = logical(),
    ..fixed = logical(),
    ..trim = logical(),
    ..orderPattern = logical()
  ),
  
  public = list(
    initialize = function(pattern = NULL, replacement = NULL, leadspace = FALSE, 
                          trailspace = FALSE, fixed = TRUE, trim = FALSE, 
                          orderPattern = fixed) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdReplaceContractions"
      private$..pattern <- pattern
      private$..replacement <- replacement
      private$..leadspace <- leadspace
      private$..trailspace <- trailspace
      private$..fixed <- fixed
      private$..trim <- trim
      private$..orderPattern <- orderPattern
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceContractions$new(x, pattern = private$..pattern, 
                                   replacement = private$..replacement,
                                   leadspace = private$..leadspace, 
                                   trailspace = private$..trailspace, 
                                   fixed = private$..fixed, 
                                   trim = private$..trim, 
                                   orderPattern = private$..orderPattern)$execute()
      return(x)
    }
  )
)