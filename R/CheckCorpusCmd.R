#------------------------------------------------------------------------------#
#                             CheckCorpusCmd                                   #
#------------------------------------------------------------------------------#
#' CheckCorpusCmd
#'
#' \code{CheckCorpusCmd} Command for the CheckCorpus class.
#'
#' Class that encapsulates the command to execute an object of the CheckCorpus
#' class
#'
#' @usage CheckCorpusCmd$new()
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
CheckCorpusCmd <- R6::R6Class(
  classname = "CheckCorpusCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,
  
  private = list(
    ..file = character()
  ),

  public = list(
    initialize = function(file = NULL) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "CheckCorpusCmd"
      private$..file <- file
      private$..logs <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- CheckCorpus$new(x, private$.file)$execute()
      return(x)
    }
  )
)
