#------------------------------------------------------------------------------#
#                                  Check Text                                  #
#------------------------------------------------------------------------------#
#' Chext
#'
#' \code{Chext} Checks text for potential anomalies.
#'
#' Checks text for a range of potential anomalies and returns a data frame 
#' containing the document, the checks performed, the frequency of the anomalies,
#' the indices to the anomalies, and a recommended course of action.
#'
#' @usage Chext$new(x)$execute()
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
Chext <- R6::R6Class(
  classname = "Chext",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextStudio0,

  private = list(

    processText = function(content) {
      content <- textclean::add_missing_endmark(x = content,
                                                replacement = private$..replacement,
                                                endmarks = private$..endmarks)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, replacement = "|", endmarks = c("?", ".", "!"), ...) {

      private$..className <- "Chext"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "Chext"
      private$..x <- x
      private$..replacement <- replacement
      private$..endmarks <- endmarks
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
