#------------------------------------------------------------------------------#
#                           CmdReplaceCurlyQuotes                              #
#------------------------------------------------------------------------------#
#' CmdReplaceCurlyQuotes
#'
#' \code{CmdReplaceCurlyQuotes} Command for the ReplaceCurlyQuotes class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceCurlyQuotes
#' class
#'
#' @usage CmdReplaceCurlyQuotes$new()
#'
#' @template textCleanParams
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
CmdReplaceCurlyQuotes <- R6::R6Class(
  classname = "CmdReplaceCurlyQuotes",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdReplaceCurlyQuotes"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceCurlyQuotes$new(x)$execute()
      return(x)
    }
  )
)
