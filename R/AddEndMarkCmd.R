#------------------------------------------------------------------------------#
#                            AddEndMarkCmd                                     #
#------------------------------------------------------------------------------#
#' AddEndMarkCmd
#'
#' \code{AddEndMarkCmd} Command for the AddEndMark class.
#'
#' Class that encapsulates the command to execute an object of the AddEndMark
#' class
#'
#' @usage AddEndMarkCmd$new(replacement = "|", endmarks = c("?", ".", "!"))
#'
#' @template textCleanParams
#' @param replacement Symbol added for missing endmarks
#' @param endmarks List of endmark symbols to detect
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
AddEndMarkCmd <- R6::R6Class(
  classname = "AddEndMarkCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCmd0,
  
  private = list(
    ..endmarks = character()
  ),

  public = list(
    initialize = function(replacement = "|", endmarks = c("?", ".", "!"), ...) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "AddEndMarkCmd"
      private$..replacement <- replacement
      private$..endmarks <- endmarks
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- AddEndMark$new(x, private$..replacement, private$..endmarks)$execute()
      return(x)
    }
  )
)
