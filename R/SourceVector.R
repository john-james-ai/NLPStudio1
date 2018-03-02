#==============================================================================#
#                                SourceVector                              #
#==============================================================================#
#' SourceVector
#'
#' \code{SourceVector} Creates Corpus objects from text sources
#'
#' @template corpusSourceStrategyClasses
#'
#' @section SourceVector Methods:
#' @template corpusSourceStrategyMethods
#'
#' @template corpusSourceStrategyParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusBuilder family of classes
#' @export
SourceVector <- R6::R6Class(
  classname = "SourceVector",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Source0,

  private = list(
    ..flat = FALSE
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Initialization Method                           #
    #-------------------------------------------------------------------------#
    initialize = function(name, corpusSource, flat = FALSE) {

      private$..source <- corpusSource
      private$..flat <- flat

      private$..className <- 'SourceVector'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()

      if (private$validateParams()$code == FALSE) stop()

      private$..corpus <- Corpus$new(name = name)

      # Create log entry
      private$..state <- paste0("Corpus Text Import object instantiated")
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                              Build Methods                              #
    #-------------------------------------------------------------------------#
    build = function() {

      private$..methodName <- "build"

      docs <- private$..source

      if (class(docs) == "character" &  private$..flat == TRUE) {

        # Create document and add content
        doc <- Document$new(paste0(private$..corpus$getName(), "-Document"))
        doc$text <- docs

        # Add document to corpus
        private$..corpus <- private$..corpus$addDocument(doc)

      } else if (class(docs) %in% c("list", "character")) {

        # Format document names
        docNames <- names(docs)
        if (is.null(docNames)) {
          docNames <- paste("Document", seq_len(length(docs)), sep = "")
        }

        for (i in 1:length(docs)) {
          doc <- Document$new(docNames[[i]])
          doc$text <- docs[[i]]
          private$..corpus <- private$..corpus$addDocument(doc)
        }
      } else {
        private$..state <- paste0("Unable to create corpus from text source. ",
                                  "The data source parameter is not a valid ",
                                  "list or character object. See ?", class(self),
                                  " for further assistance.")
        self$logIt("Error")
        stop()
      }

      private$..state <- paste0("Created corpus ", private$..meta[["name"]], " from text.")
      self$logIt()

      invisible(self)
    },

    getResult = function() {

      private$..methodName <- 'getResult'
      private$..state <- "Returned corpus object to calling environment"
      self$logIt()
      return(private$..corpus)
    },

    #-------------------------------------------------------------------------#
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$corpusSourceVector(self)
    }
  )
)
