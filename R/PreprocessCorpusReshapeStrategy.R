#==============================================================================#
#                       PreprocessCorpusReshapeStrategy                        #
#==============================================================================#
#' PreprocessCorpusReshapeStrategy
#'
#' \code{PreprocessCorpusReshapeStrategy} Class responsible for reshaping a Corpus into sentences.
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
PreprocessCorpusReshapeStrategy <- R6::R6Class(
  classname = "PreprocessCorpusReshapeStrategy",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = PreprocessCorpusStrategy0,

  public = list(

    initialize = function(object, name = NULL) {

      private$..className <- "PreprocessCorpusReshapeStrategy"
      private$..methodName <- "initialize"
      private$..in <- object
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

      # Create new Corpus object
      if (is.null(name)) name <- object$getName()
      private$..out <- Corpus$new(name = name)
      private$..out <- private$cloneCorpus(private$..in, private$..out)

      # log
      private$..state <- paste0("Successfully initialized PreprocessCorpusReshapeStrategy class object.")
      self$logIt()

      invisible(self)
    },

    preprocess = function() {

      private$..methodName <- "preprocess"

      docs <- private$..in$getDocuments()
      lapply(docs, function(d) {
        name <- d$getName()
        doc <- PreprocessDocumentReshapeStrategy$new(object = d,
                                                 name = name)$preprocess()$getResult()
        private$..out$addDocument(doc)
      })

      # log
      private$..state <- paste0("Successfully performed PreprocessCorpusReshapeStrategy.")
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
      visitor$preprocessCorpusReshapeStrategy(self)
    }
  )
)
