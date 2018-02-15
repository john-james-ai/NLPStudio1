#------------------------------------------------------------------------------#
#                             SplitDocumentCmd                                 #
#------------------------------------------------------------------------------#
#' SplitDocumentCmd
#'
#' \code{SplitDocumentCmd} Command for the SplitDocument class.
#'
#' Class that encapsulates the command to execute an object of the SplitDocument
#' class
#'
#' @usage SplitDocumentCmd$new(trainSize = .7, valSize = .15, testSize = .15, seed = 232)
#' 
#' @template textStudioParams
#' @param trainSize Numeric between 0 and 1, indicating the proportion of the data set to include in the training set
#' @param valSize Numeric between 0 and 1, indicating the proportion of the data set to include in the validation set
#' @param testSize Numeric between 0 and 1, indicating the proportion of the data set to include in the test set
#' @param seed Numeric seed used by the sample function.
#' @template textStudioMethods
#' @template textStudioClasses
#' @template textStudioDesign
#'
#' @docType class
#' @author John James, \email{jjames@@dataScienceSalon.org}
#' @family TextStudio Classes
#' @export
SplitDocumentCmd <- R6::R6Class(
  classname = "SplitDocumentCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Split0,
  
  public = list(
    initialize = function() {
      private$..methodName <- "initialize"
      private$..meta[["name"]] <- "SplitDocumentCmd"
      private$..trainSize <- trainSize
      private$..valSize <- valSize
      private$..testSize <- testSize
      private$..seed <- seed
      private$..logs <- LogR$new()
      invisible(self)
    },
    execute = function(x) {
      x <- SplitDocument$new(x, trainSize = private$..trainSize,
                           valSize = private$..valSize,
                           testSize = private$..testSize,
                           seed = private$..seed)$execute()
      return(x)
    }
  )
)
