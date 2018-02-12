#------------------------------------------------------------------------------#
#                            CmdRemovePunct                                    #
#------------------------------------------------------------------------------#
#' CmdRemovePunct
#'
#' \code{CmdRemovePunct} Command for the RemovePunct class.
#'
#' Class that encapsulates the command to execute an object of the RemovePunct
#' class
#'
#' @usage CmdRemovePunct$new(endmark = FALSE, apostrophe = FALSE)
#'
#' @template textCleanParams
#' @param endmark Logical indicating whether to remove endmarks.
#' @param apostrophe Logical indicating whether to remove apostrophes.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
CmdRemovePunct <- R6::R6Class(
  classname = "CmdRemovePunct",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CmdText0,

  private = list(
    ..endmark = logical(),
    ..apostrophe = logical()
  ),

  public = list(
    initialize = function(endmark = FALSE, apostrophe = FALSE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CmdRemovePunct"
      private$..endmark <- endmark
      private$..apostrophe <- apostrophe

      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- RemovePunct$new(x, endmark = private$..endmark,
                                 apostrophe = private$..apostrophe)$execute()
      return(x)
    }
  )
)
