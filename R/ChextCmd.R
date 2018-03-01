#------------------------------------------------------------------------------#
#                               ChextCmd                                       #
#------------------------------------------------------------------------------#
#' ChextCmd
#'
#' \code{ChextCmd} Command for the Chext class.
#'
#' Class that encapsulates the command to execute an object of the Chext
#' class
#'
#' @usage ChextCmd$new(joinTokens = FALSE, remove = FALSE)
#'
#' @template textStudioParams
#' @param what Character string containing either c('character', 'word' ,'sentence)
#' indicating to which format the document should be tokenized.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ChextCmd <- R6::R6Class(
  classname = "ChextCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  public = list(
    initialize = function(what) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "ChextCmd"
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- Chext$new(x)$execute()
      return(x)
    }
  )
)
