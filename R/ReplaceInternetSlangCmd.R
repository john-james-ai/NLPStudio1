#------------------------------------------------------------------------------#
#                          ReplaceInternetSlangCmd                             #
#------------------------------------------------------------------------------#
#' ReplaceInternetSlangCmd
#'
#' \code{ReplaceInternetSlangCmd} Command for the ReplaceInternetSlang class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceInternetSlang
#' class
#'
#' @usage ReplaceInternetSlangCmd$new(slang = NULL, replacement = NULL, ignoreCase = TRUE)
#'
#' @template textCleanParams
#' @param slang A vector of slang strings to replace.
#' @param replacement A vector of strings with which to replace slang
#' @param ignoreCase Logical. If TRUE the case of slang will be ignored (replacement regardless of case)
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceInternetSlangCmd <- R6::R6Class(
  classname = "ReplaceInternetSlangCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCmd0,

  private = list(
    ..slang = character(),
    ..ignoreCase = logical()
  ),

  public = list(
    initialize = function(slang = NULL, replacement = NULL, ignoreCase = TRUE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "ReplaceInternetSlangCmd"
      private$..slang <- slang
      private$..replacement <- replacement
      private$..ignoreCase <- ignoreCase
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceInternetSlang$new(x, slang = private$..slang,
                                    replacement = private$..replacement,
                                    ignoreCase = private$..ignoreCase)$execute()
      return(x)
    }
  )
)
