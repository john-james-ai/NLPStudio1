#------------------------------------------------------------------------------#
#                               CmdReplaceNames                                #
#------------------------------------------------------------------------------#
#' CmdReplaceNames
#'
#' \code{CmdReplaceNames} Command for the ReplaceNames class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceNames
#' class
#'
#' @usage CmdReplaceNames$new(names = NULL, replacement = NULL)
#'
#' @template textCleanParams
#' @param names Vector of names to replace.
#' @param replacement A string with which to replace names.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
CmdReplaceNames <- R6::R6Class(
  classname = "CmdReplaceNames",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  private = list(
    ..names = character()
  ),

  public = list(
    initialize = function(names = NULL, replacement = NULL) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdReplaceNames"
      private$..names <- names
      private$..replacement <- replacement
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceNames$new(x, names = private$..names,
                            replacement = private$..replacement)$execute()
      return(x)
    }
  )
)
