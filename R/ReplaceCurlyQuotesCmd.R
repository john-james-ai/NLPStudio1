#------------------------------------------------------------------------------#
#                           ReplaceCurlyQuotesCmd                              #
#------------------------------------------------------------------------------#
#' ReplaceCurlyQuotesCmd
#'
#' \code{ReplaceCurlyQuotesCmd} Command for the ReplaceCurlyQuotes class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceCurlyQuotes
#' class
#'
#' @usage ReplaceCurlyQuotesCmd$new()
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
ReplaceCurlyQuotesCmd <- R6::R6Class(
  classname = "ReplaceCurlyQuotesCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCmd0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "ReplaceCurlyQuotesCmd"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceCurlyQuotes$new(x)$execute()
      return(x)
    }
  )
)
