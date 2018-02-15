#------------------------------------------------------------------------------#
#                                Lower Case                                    #
#------------------------------------------------------------------------------#
#' LowerCase
#'
#' \code{LowerCase}  Converts all alphabetic characters to lower case.
#'
#' A wrapper for \code{\link{tolower}} which converts alphabetic characters
#' to lower case.
#'
#' @usage LowerCase$new(x)$execute()
#'
#' @template textStudioParams
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @examples
#'
#' @return \code{LowerCase} Returns a vector with alphabetic characters converted to lower case.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
LowerCase <- R6::R6Class(
  classname = "LowerCase",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Text0,

  private = list(

    processText = function(content) {
      content <- tolower(x = content)
      return(content)
    }
  ),

  public = list(
    initialize = function(x) {
      private$..className <- "LowerCase"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "LowerCase"
      private$..x <- x
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
