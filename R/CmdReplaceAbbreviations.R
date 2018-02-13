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
#' @param abbreviations A two column key of abbreviations (column 1) and long form replacements (column 2) or a vector of abbreviations.
#' Default is to use qdapDictionaries's abbreviations data set.
#' @param replacement Vector of long form replacements if a data frame is not supplied to the abbreviation argument.
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
    ..abbreviations = character(),
    ..replacement = character()
  ),

  public = list(
    initialize = function(abbreviations = NULL, replacement = NULL, ignoreCase = TRUE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdReplaceAbbreviations"
      private$..abbreviations <- abbreviations
      private$..replacement <- replacement
      private$..ignoreCase <- ignoreCase
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceAbbreviations$new(x, abbreviations = private$..abbreviations,
                                   replacement = private$..replacement)$execute()
      return(x)
    }
  )
)
