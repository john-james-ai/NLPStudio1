#------------------------------------------------------------------------------#
#                               TokenizeCmd                                    #
#------------------------------------------------------------------------------#
#' TokenizeCmd
#'
#' \code{TokenizeCmd} Command for the Tokenize class.
#'
#' Class that encapsulates the command to execute an object of the Tokenize
#' class
#'
#' @usage TokenizeCmd$new(joinTokens = FALSE, remove = FALSE)
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
TokenizeCmd <- R6::R6Class(
  classname = "TokenizeCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  

  private = list(
    ..what = character()
  ),

  public = list(
    initialize = function(what) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "TokenizeCmd"
      private$..what <- what
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- Tokenize$new(x, what = private$..what)$execute()
      return(x)
    }
  )
)
