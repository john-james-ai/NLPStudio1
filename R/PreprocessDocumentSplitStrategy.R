#==============================================================================#
#                       PreprocessDocumentSplitStrategy                        #
#==============================================================================#
#' PreprocessDocumentSplitStrategy
#'
#' \code{PreprocessDocumentSplitStrategy} Class responsible splitting a Document object into train, test, and an optional validation set.
#'
#' Class responsible for reshaping a Document class object into tokens, sentences, or paragraphs.
#'
#' @template preprocessClasses
#' @template preprocessMethods
#' @template preprocessParams
#' @param trainSize Numeric between 0 and 1, indicating the proportion of the data set to include in the training set
#' @param valSize Numeric between 0 and 1, indicating the proportion of the data set to include in the validation set
#' @param testSize Numeric between 0 and 1, indicating the proportion of the data set to include in the test set
#' @param seed Numeric seed used by the sample function.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Preprocess Family of Classes
#' @export
PreprocessDocumentSplitStrategy <- R6::R6Class(
  classname = "PreprocessDocumentSplitStrategy",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = PreprocessDocumentStrategy0,

  private = list(
    ..trainSize = 0,
    ..valSize = 0,
    ..testSize = 0,
    ..seed = numeric()
  ),

  public = list(

    initialize = function(object, trainSize, valSize = 0, testSize, name = NULL, seed = NULL)  {

      private$..className <- "PreprocessDocumentSplitStrategy"
      private$..methodName <- "initialize"
      private$..in <- object
      private$..trainSize <- trainSize
      private$..valSize <- valSize
      private$..testSize <- testSize
      private$..seed <- seed
      private$..logs <- LogR$new()

      # Validate input
      if (!("Document" %in% class(object))) {
        private$..state <- paste0("Invalid object for this Preprocess Class.  ",
                                  "This class preprocesses objects of the Document ",
                                  "class only.  See ?", class(self)[1],
                                  " for further assistance.")
        self$logIt("Error")
        stop()
      }

      # Confirm splits sum to one.
      if (sum(trainSize, valSize, testSize) != 1) {
        private$..state <- paste0("Unable to perform split operation. ",
                                  "The sum of proportions must equal one. ",
                                  "See ?", class(self)[1], " for further assistance.")
        self$logIt("Error")
        stop()
      }

      # log
      private$..state <- paste0("Successfully initialized PreprocessDocumentSplitStrategy class object.")
      self$logIt()

      invisible(self)
    },

    preprocess = function() {

      # Obtain content
      content <- private$..in$read()

      # Set seed
      if (!is.null(private$..seed)) {
        set.seed(private$..seed)
      }

      # Establish sample indices
      proportions <- c(private$..trainSize, private$..valSize, private$..testSize)
      x <- c(1:length(proportions))
      ss <- sample(x, size = length(content), replace = TRUE, prob = proportions)

      # Create training set
      train <- Document$new(name = private$..in$getName())
      train <- private$cloneDocument(private$..in, train)
      train$content <- content[ss==1]
      private$..out[["train"]] <- train

      # Create validation set
      if (private$..valSize > 0) {
        val <- Document$new(name = private$..in$getName())
        val <- private$cloneDocument(private$..in, val)
        val$content <- content[ss==2]
        private$..out[["validation"]] <- val
      }

      # Create test set
      test <- Document$new(name = private$..in$getName())
      test <- private$cloneDocument(private$..in, test)
      test$content <- content[ss==3]
      private$..out[["test"]] <- test

      # log
      private$..state <- paste0("Successfully split Document")
      self$logIt()

      invisible(self)
    },


    getResult = function() {
      return(private$..out)
    },

    #-------------------------------------------------------------------------#
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$processDocumentSplitStrategy(self)
    }
  )
)
