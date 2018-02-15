#------------------------------------------------------------------------------#
#                            RemoveSymbolsCmd                                  #
#------------------------------------------------------------------------------#
#' RemoveSymbolsCmd
#'
#' \code{RemoveSymbolsCmd} Command for the RemoveSymbols class.
#'
#' Class that encapsulates the command to execute an object of the RemoveSymbols
#' class
#'
#' @usage RemoveSymbolsCmd$new()
#'
#' @template textStudioParams
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
RemoveSymbolsCmd <- R6::R6Class(
  classname = "RemoveSymbolsCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCmd0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "RemoveSymbolsCmd"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemoveSymbols$new(x)$execute()
      return(x)
    }
  )
)
