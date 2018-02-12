#------------------------------------------------------------------------------#
#                             Replace Names                                    #
#------------------------------------------------------------------------------#
#' ReplaceNames
#'
#' \code{ReplaceNames}  Replaces first/last names.
#'
#' A wrapper for \code{\link[textclean]{replace_names}}
#' Replaces first and last names.
#' Source \url{https://cran.r-project.org/web/packages/textclean/textclean.pdf}
#'
#' @usage ReplaceNames$new(x, names = NULL, replace = NULL)$execute()
#'
#' @template textCleanParams
#' @param names Vector of names to replace.
#' @param replace A string with which to replace names.
#' @template textCleanMethods
#' @template textCleanClasses
#' @template textCleanDesign
#'
#' @examples
#'
#' @return \code{ReplaceNames} Returns a vector with names replaced.
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextClean Classes
#' @export
ReplaceNames <- R6::R6Class(
  classname = "ReplaceNames",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = TextClean0,

  private = list(

    ..names = character(),

    processText = function(content) {
      content <- textclean::replace_names(x = content,
                                          names = private$..names,
                                          replacement = private$..replace)
      return(content)
    }
  ),

  public = list(
    initialize = function(x, names = NULL, replace = NULL) {
      private$..className <- "ReplaceNames"
      private$..methodName <- "initialize"
      private$..meta[["name"]] <-  "ReplaceNames"
      private$..x <- x
      private$..names <- names
      private$..replace <- replace
      private$..logs  <- LogR$new()
      invisible(self)
    }
  )
)
