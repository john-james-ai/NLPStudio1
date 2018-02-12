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
#' @param contractions A two column hash of contractions (column 1) and expanded
#' form replacements (column 2).  Default is to use \code{\link[lexicon]{key_contractions}} data set.
#' @param ignoreCase logical.  Should case be ignored?
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
    ..contractions = character(),
    ..ignoreCase = logical()
  ),

  public = list(
    initialize = function(contractions = NULL, ignoreCase = TRUE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdReplaceContractions"
      private$..contractions <- contractions
      private$..ignoreCase <- ignoreCase
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceContractions$new(x, contractions = private)$execute()
      return(x)
    }
  )
)
