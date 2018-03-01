#==============================================================================#
#                             SplitDocument                                    #
#==============================================================================#
#' SplitDocument
#'
#'
#' \code{SplitDocument} Splits a Document object into training, test and optionally, validation sets
#'
#' Splits a Document object into training, test, and optionally, validation sets.
#'
#' @usage SplitDocument$new(x, trainSize = .7, valSize = .15, testSize = .15, seed = 232)$execute()$getResult()
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
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family TextStudio Classes
#' @export
SplitDocument <- R6::R6Class(
  classname = "SplitDocument",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Split0,
  
  private = list(
    cloneDocument = function(inDocument, outDocument) {
      
      keys <- names(as.list(inDocument$meta()))
      keys <- keys[keys!= "name"]
      values <- as.list(inDocument$meta())
      values["name"] <- NULL
      lapply(seq_along(keys), function(k) {
        outDocument$meta(key = keys[[k]], value = values[[k]])
      })
      
      return(outDocument)
    }
  ),

  public = list(

    initialize = function(x, trainSize, valSize = 0, testSize, name = NULL, seed = NULL)  {

      private$..className <- "SplitDocument"
      private$..methodName <- "initialize"
      private$..x <- x
      private$..name <- name
      private$..trainSize <- trainSize
      private$..valSize <- valSize
      private$..testSize <- testSize
      private$..seed <- seed
      private$..logs <- LogR$new()

      if (private$validateParams()$code == FALSE) stop()

      # Confirm splits sum to one.
      if (sum(trainSize, valSize, testSize) != 1) {
        private$..state <- paste0("Unable to perform split operation. ",
                                  "The sum of proportions must equal one. ",
                                  "See ?", class(self)[1], " for further assistance.")
        self$logIt("Error")
        stop()
      }

      # log
      private$..state <- paste0("Successfully initialized SplitDocument class object.")
      self$logIt()

      invisible(self)
    },

    execute = function() {
      
      # Obtain content
      content <- private$..x$text
      
      # Set seed
      if (!is.null(private$..seed)) {
        set.seed(private$..seed)
      }

      # Establish sample indices
      proportions <- c(private$..trainSize, private$..valSize, private$..testSize)
      x <- c(1:length(proportions))
      ss <- sample(x, size = length(content), replace = TRUE, prob = proportions)
      
      # Format name
      name <- ifelse(is.null(private$..name), private$..x$getName(), private$..name)

      # Create training set
      if (private$..trainSize > 0) {
        train <- Document$new(name = paste0(name, ".train"))
        train <- private$cloneDocument(private$..x, train)
        train$text <- content[ss==1]
        private$..cvSet[["train"]] <- train
      }

      # Create validation set
      if (private$..valSize > 0) {
        val <- Document$new(name = paste0(name, ".validation"))
        val <- private$cloneDocument(private$..x, val)
        val$text <- content[ss==2]
        private$..cvSet[["validation"]] <- val
      }

      # Create test set
      if (private$..testSize > 0) {
        test <- Document$new(name = paste0(name, ".test"))
        test <- private$cloneDocument(private$..x, test)
        test$text <- content[ss==3]
        private$..cvSet[["test"]] <- test
      }

      # log
      private$..state <- paste0("Successfully split Document")
      self$logIt()

      invisible(self)
    },


    getResult = function() {
      return(private$..cvSet)
    },

    #-------------------------------------------------------------------------#
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$splitDocument(self)
    }
  )
)
