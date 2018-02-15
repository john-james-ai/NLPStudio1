#------------------------------------------------------------------------------#
#                            RemovePunctCmd                                    #
#------------------------------------------------------------------------------#
#' RemovePunctCmd
#'
#' \code{RemovePunctCmd} Command for the RemovePunct class.
#'
#' Class that encapsulates the command to execute an object of the RemovePunct
#' class
#'
#' @usage RemovePunctCmd$new(endmark = FALSE, apostrophe = FALSE)
#'
#' @template textStudioParams
#' @param endmark Logical indicating whether to remove endmarks.
#' @param apostrophe Logical indicating whether to remove apostrophes.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
RemovePunctCmd <- R6::R6Class(
  classname = "RemovePunctCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextCmd0,

  private = list(
    ..endmark = logical(),
    ..apostrophe = logical()
  ),

  public = list(
    initialize = function(endmark = FALSE, apostrophe = FALSE) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "RemovePunctCmd"
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
