#------------------------------------------------------------------------------#
#                          CmdReplaceInternetSlang                             #
#------------------------------------------------------------------------------#
#' CmdReplaceInternetSlang
#'
#' \code{CmdReplaceInternetSlang} Command for the ReplaceInternetSlang class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceInternetSlang
#' class
#'
#' @usage CmdReplaceInternetSlang$new(slang = NULL, replace = NULL, ignoreCase = TRUE)
#'
#' @template textCleanParams
#' @param slang A vector of slang strings to replace.
#' @param replace A vector of strings with which to replace slang
#' @param ignoreCase Logical. If TRUE the case of slang will be ignored (replacement regardless of case)
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
CmdReplaceInternetSlang <- R6::R6Class(
  classname = "CmdReplaceInternetSlang",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  private = list(
    ..slang = character(),
    ..ignoreCase = logical()
  ),

  public = list(
    initialize = function(slang = NULL, replace = NULL, ignoreCase = TRUE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdReplaceInternetSlang"
      private$..slang <- slang
      private$..replace <- replace
      private$..ignoreCase <- ignoreCase
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceInternetSlang$new(x, slang = private$..slang,
                                    replace = private$..replace,
                                    ignoreCase = private$..ignoreCase)$execute()
      return(x)
    }
  )
)
