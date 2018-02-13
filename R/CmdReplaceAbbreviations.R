#------------------------------------------------------------------------------#
#                           CmdReplaceAbbreviations                            #
#------------------------------------------------------------------------------#
#' CmdReplaceAbbreviations
#'
#' \code{CmdReplaceAbbreviations} Command for the ReplaceAbbreviations class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceAbbreviations
#' class
#'
#' @usage CmdReplaceAbbreviations$new(abbreviation, replacement = NULL, ignoreCase = TRUE )
#'
#' @template textCleanParams
#' @param abbreviation A two column key of abbreviations (column 1) and long form replacements (column 2) or a vector of abbreviations.
#' Default is to use qdapDictionaries's abbreviations data set.
#' @param replacement Vector of long form replacements if a data frame is not supplied to the abbreviation argument.
#' @param ignoreCase Logical. If TRUE replaces without regard to capitalization.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
CmdReplaceAbbreviations <- R6::R6Class(
  classname = "CmdReplaceAbbreviations",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  private = list(
    ..abbreviation = character(),
    ..replacement = character(),
    ..ignoreCase = logical()
  ),

  public = list(
    initialize = function(abbreviation, replace, ignoreCase = TRUE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdReplaceAbbreviations"
      private$..abbreviation <- abbreviation
      private$..replacement <- replacement
      private$..ignoreCase <- ignoreCase
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceAbbreviations$new(x, abbreviation = private$..abbreviation,
                                   replacement = private$..replacement,
                                   ignoreCase = private$..ignoreCase)$execute()
      return(x)
    }
  )
)
