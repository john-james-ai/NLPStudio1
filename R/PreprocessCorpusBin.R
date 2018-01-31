#==============================================================================#
#                           PreprocessCorpusBin                                #
#==============================================================================#
#' PreprocessCorpusBin
#'
#' \code{PreprocessCorpusBin} Class responsible for performing repairs of control characters in Corpus objects.
#'
#' Class repairs control character representations in Corpus objects.
#'
#' @template preprocessClasses.R
#' @template preprocessMethods.R
#' @template preprocessParams.R
#' @template preprocessPreprocessParams.R
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family PrePreprocess Family of Classes
#' @export
PreprocessCorpusBin <- R6::R6Class(
  classname = "PreprocessCorpusBin",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = PreprocessCorpus0,

  public = list(

    initialize = function(object, name, substitutions = NULL) {

      private$..className <- "PreprocessCorpusBin"
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

      if (is.null(substitutions)) {
        private$..substitutions <- NLPStudio:::ctrl
      } else {
        private$..substitutions <- substitutions
      }


      # Create new Corpus object
      private$..out <- Corpus$new(name = name)
      private$..out <- private$cloneCorpus(private$..in, private$..out)

      # log
      private$..state <- paste0("Successfully initialized PreprocessCorpusBin class object.")
      self$logIt()

      invisible(self)
    },

    preprocess = function() {

      private$..methodName <- "preprocess"

      docs <- private$..in$getDocuments()
      lapply(docs, function(d) {
        name <- d$getName()
        doc <- PreprocessDocumentBin$new(object = d, name = name)$preprocess()$getResult()
        private$..out$addDocument(doc)
      })

      # log
      private$..state <- paste0("Successfully performed PreprocessCorpusBin.")
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
      visitor$preprocessCorpusBin(self)
    }
  )
)
