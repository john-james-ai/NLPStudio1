#------------------------------------------------------------------------------#
#                           CmdReplaceWordElongation                           #
#------------------------------------------------------------------------------#
#' CmdReplaceWordElongation
#'
#' \code{CmdReplaceWordElongation} Command for the ReplaceWordElongation class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceWordElongation
#' class
#'
#' @usage CmdReplaceWordElongation$new(impartMeaning = TRUE)
#'
#' @template textCleanParams
#' @param impartMeaning logical. If TRUE, known elongation semantics are used as replacements
#' (see textclean:::meaning_elongations for known elongation semantics and replacements).
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
CmdReplaceWordElongation <- R6::R6Class(
  classname = "CmdReplaceWordElongation",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  private = list(
    ..impartMeaning = logical()
  ),

  public = list(
    initialize = function(impartMeaning = FALSE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdReplaceWordElongation"
      private$..impartMeaning <- impartMeaning
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceWordElongation$new(x, impartMeaning = private$..impartMeaning)$execute()
      return(x)
    }
  )
)
