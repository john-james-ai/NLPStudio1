#------------------------------------------------------------------------------#
#                               ReplaceNamesCmd                                #
#------------------------------------------------------------------------------#
#' ReplaceNamesCmd
#'
#' \code{ReplaceNamesCmd} Command for the ReplaceNames class.
#'
#' Class that encapsulates the command to execute an object of the ReplaceNames
#' class
#'
#' @usage ReplaceNamesCmd$new(names = NULL, replacement = NULL)
#'
#' @template textStudioParams
#' @param names Vector of names to replace.
#' @param replacement A string with which to replace names.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
ReplaceNamesCmd <- R6::R6Class(
  classname = "ReplaceNamesCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  

  private = list(
    ..names = character()
  ),

  public = list(
    initialize = function(names = NULL, replacement = NULL) {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "ReplaceNamesCmd"
      private$..names <- names
      private$..replacement <- replacement
      private$..logs  <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- ReplaceNames$new(x, names = private$..names,
                            replacement = private$..replacement)$execute()
      return(x)
    }
  )
)
