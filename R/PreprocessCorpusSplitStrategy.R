#==============================================================================#
#                       PreprocessCorpusSplitStrategy                          #
#==============================================================================#
#' PreprocessCorpusSplitStrategy
#'
#' \code{PreprocessCorpusSplitStrategy} Class responsible for reshaping a Corpus into sentences.
#'
#' Class reshapes Corpus object text into sentence vectors.
#'
#' @template preprocessClasses
#' @template preprocessMethods
#' @template preprocessParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Preprocess Family of Classes
#' @export
PreprocessCorpusSplitStrategy <- R6::R6Class(
  classname = "PreprocessCorpusSplitStrategy",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = PreprocessCorpusStrategy0,

  private = list(
    ..trainSize = 0,
    ..valSize = 0,
    ..testSize = 0,
    ..seed = numeric()
  ),

  public = list(

    initialize = function(object, trainSize, valSize = 0, testSize, name = NULL, seed = NULL) {

      private$..className <- "PreprocessCorpusSplitStrategy"
      private$..methodName <- "initialize"
      private$..methodName <- "initialize"
      private$..in <- object
      private$..trainSize <- trainSize
      private$..valSize <- valSize
      private$..testSize <- testSize
      private$..seed <- seed
      private$..logs <- LogR$new()

      # Validate input
      if (!("Corpus" %in% class(object))) {
        private$..state <- paste0("Invalid object for this Preprocess Class.  ",
                                  "This class preprocesses objects of the Corpus ",
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
      private$..state <- paste0("Successfully initialized PreprocessCorpusSplitStrategy class object.")
      self$logIt()

      invisible(self)
    },

    preprocess = function() {

      private$..methodName <- "preprocess"

      # Split Documents
      docs <- private$..in$getDocuments()
      cvSets <- lapply(docs, function(d) {
        cvSet <- PreprocessDocumentSplitStrategy$new(object = d,
                                                   trainSize = private$..trainSize,
                                                   valSize = private$..valSize,
                                                   testSize = private$..testSize,
                                                   seed = private$..seed)$preprocess()$getResult()
        names(cvSet) <- d$getName()
        cvSet
      })

      # Create new corpora
      if (private$..trainSize > 0 ) {
        trainCorpus <- Corpus$new(name = private$..in$getName)
        for (i in 1:length(cvSets)) {
          private$..out[["train"]] <- trainCorpus$addDocument(cvSets[[i]]$train)
        }
      }
      if (private$..valSize > 0 ) {
        validationCorpus <- Corpus$new(name = private$..in$getName)
        for (i in 1:length(cvSets)) {
          private$..out[["validation"]] <- validationCorpus$addDocument(cvSets[[i]]$validation)
        }
      }
      if (private$..testSize > 0 ) {
        testCorpus <- Corpus$new(name = private$..in$getName)
        for (i in 1:length(cvSets)) {
          private$..out[["test"]] <- testCorpus$addDocument(cvSets[[i]]$test)
        }
      }

      # log
      private$..state <- paste0("Successfully performed PreprocessCorpusSplitStrategy.")
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
      visitor$preprocessCorpusSplitStrategy(self)
    }
  )
)
