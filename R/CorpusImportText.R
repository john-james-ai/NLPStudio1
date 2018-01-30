#==============================================================================#
#                                CorpusImportText                              #
#==============================================================================#
#' CorpusImportText
#'
#' \code{CorpusImportText} Creates Corpus objects from text sources
#'
#' @template corpusImportStrategyClasses
#'
#' @section CorpusImportText Methods:
#' @template corpusImportStrategyMethods
#'
#' @template corpusImportStrategyParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusBuilder family of classes
#' @export
CorpusImportText <- R6::R6Class(
  classname = "CorpusImportText",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CorpusImport0,

  private = list(
    ..flat = FALSE
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Initialization Method                           #
    #-------------------------------------------------------------------------#
    initialize = function(name, dataSource, flat = FALSE) {

      private$..name <- name
      private$..dataSource <- dataSource
      private$..flat <- flat

      private$..className <- 'CorpusImportText'
      private$..methodName <- 'initialize'
      private$..state <- paste0("CorpusImportText object instantiated.")
      private$..logs <- LogR$new()

      private$..corpus <- Corpus$new(name = name)

      # Create log entry
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                              Build Methods                              #
    #-------------------------------------------------------------------------#
    build = function() {

      private$..methodName <- "build"

      docs <- private$..dataSource

      if (class(docs) == "character" &  private$..flat == TRUE) {

        # Create document and add content
        doc <- Document$new(private$..name)
        doc$content <- docs

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
          doc$content <- docs[[i]]
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

      private$..state <- paste0("Created corpus ", private$..name, " from text.")
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
      visitor$corpusBuilder(self)
    }
  )
)
