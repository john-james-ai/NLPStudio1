#------------------------------------------------------------------------------#
#                               ReplaceKernCmd                                 #
#------------------------------------------------------------------------------#
#' ReplaceKernCmd
#'
#' \code{ReplaceKernCmd} Command for the ReplaceKern class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceKern
#' class
#'
#' @usage ReplaceKernCmd$new()
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
ReplaceKernCmd <- R6::R6Class(
  classname = "ReplaceKernCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCmd0,

  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "ReplaceKernCmd"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceKern$new(x)$execute()
      return(x)
    }
  )
)
