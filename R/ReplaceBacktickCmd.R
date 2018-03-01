#------------------------------------------------------------------------------#
#                           ReplaceBacktickCmd                                 #
#------------------------------------------------------------------------------#
#' ReplaceBacktickCmd
#'
#' \code{ReplaceBacktickCmd} Command for the ReplaceBacktick class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceBacktick
#' class
#'
#' @usage ReplaceBacktickCmd$new()
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
ReplaceBacktickCmd <- R6::R6Class(
  classname = "ReplaceBacktickCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,  


  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "ReplaceBacktickCmd"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceBacktick$new(x)$execute()
      return(x)
    }
  )
)
