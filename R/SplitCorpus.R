#==============================================================================#
#                               SplitCorpus                                    #
#==============================================================================#
#' SplitCorpus
#'
#' \code{SplitCorpus} Splits corpus into training, test and optionally, validation sets
#'
#' Class reshapes Corpus object text into sentence vectors.
#'
#' @usage SplitCorpus$new(x, trainSize = .7, valSize = .15, testSize = .15, seed = 232)$execute()$getResult()
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
SplitCorpus <- R6::R6Class(
  classname = "SplitCorpus",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Split0,
  
  private = list(
    cloneCorpus = function(inCorpus, outCorpus) {
      
      keys <- names(as.list(inCorpus$meta()))
      keys <- keys[keys!= "name"]
      values <- as.list(inCorpus$meta())
      values["name"] <- NULL
      lapply(seq_along(keys), function(k) {
        outCorpus$meta(key = keys[[k]], value = values[[k]])
      })
      
      return(outCorpus)
    }
  ),

  public = list(

    initialize = function(x, trainSize, valSize = 0, testSize, name = NULL, seed = NULL) {

      private$..className <- "SplitCorpus"
      private$..methodName <- "initialize"
      private$..x <- x
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
      private$..state <- paste0("Successfully initialized SplitCorpus class object.")
      self$logIt()

      invisible(self)
    },

    execute = function() {

      private$..methodName <- "execute"

      # Split Documents
      docs <- private$..x$getDocuments()
      cvSets <- lapply(docs, function(d) {
        cvSet <- SplitDocument$new(x =  d,
                                   trainSize = private$..trainSize,
                                   valSize = private$..valSize,
                                   testSize = private$..testSize,
                                   seed = private$..seed)$execute()$getResult()
      })

      # Create new corpora
      if (private$..trainSize > 0 ) {
        trainCorpus <- Corpus$new(name = private$..x$getName())
        trainCorpus <- private$cloneCorpus(private$..x, trainCorpus)
        for (i in 1:length(cvSets)) {
          private$..cvSet[["train"]] <- trainCorpus$addDocument(cvSets[[i]]$train)
        }
      }
      if (private$..valSize > 0 ) {
        validationCorpus <- Corpus$new(name = private$..x$getName())
        validationCorpus <- private$cloneCorpus(private$..x, validationCorpus)
        for (i in 1:length(cvSets)) {
          private$..cvSet[["validation"]] <- validationCorpus$addDocument(cvSets[[i]]$validation)
        }
      }
      if (private$..testSize > 0 ) {
        testCorpus <- Corpus$new(name = private$..x$getName())
        testCorpus <- private$cloneCorpus(private$..x, testCorpus)
        for (i in 1:length(cvSets)) {
          private$..cvSet[["test"]] <- testCorpus$addDocument(cvSets[[i]]$test)
        }
      }

      # log
      private$..state <- paste0("Successfully performed SplitCorpus.")
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
      visitor$splitCorpus(self)
    }
  )
)
