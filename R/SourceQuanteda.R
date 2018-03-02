#==============================================================================#
#                            SourceQuanteda                              #
#==============================================================================#
#' SourceQuanteda
#'
#' \code{SourceQuanteda} Creates Corpus objects from Quanteda corpus objects.
#'
#' @template corpusSourceStrategyClasses
#'
#' @section SourceQuanteda Methods:
#' @template corpusSourceStrategyMethods
#'
#' @template corpusSourceStrategyParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusBuilder family of classes
#' @export
SourceQuanteda <- R6::R6Class(
  classname = "SourceQuanteda",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Source0,

  public = list(

    #-------------------------------------------------------------------------#
    #                       Initialization Method                             #
    #-------------------------------------------------------------------------#
    initialize = function(name, corpusSource) {

      private$..source <- corpusSource

      private$..className <- 'SourceQuanteda'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()

      if (private$validateParams()$code == FALSE) stop()

      private$..corpus <- Corpus$new(name = name)

      # Create log entry
      private$..state <- paste0("Corpus Quanteda Import object instantiated")
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                              Build Methods                              #
    #-------------------------------------------------------------------------#
    build = function() {

      private$..methodName <- "build"

      # Extract data and metadata
      texts <- private$..source$documents[["texts"]]
      metaData <- private$..source$documents[-which(names(private$..source$documents) == "texts")]
      rownames(metaData) <- NULL

      # Extract a name variable
      if (is.null(metaData$name)) {
        if (is.null(metaData$doc_id)) {
          names <- paste0(private$..meta[["name"]], "-document-", seq(1:length(texts)))
        } else {
          names <- metaData$doc_id
          metaData <- metaData %>% select(-doc_id)
        }
      } else {
        names <- metaData$name
        metaData <- metaData %>% select(-name)
      }

      # Create corpus
      for (i in 1:length(texts)) {
        doc <- Document$new(name = names[i])  # Create document
        keys <- names(metaData[i,])                   # Format metadata key value pairs
        values <- metaData[i,]
        for (j in 1:length(keys)) {                   # Update metadata for document
          doc <- doc$meta(key = keys[j], value = values[j])
        }
        doc$text <- texts[i]                       # Add content
        private$..corpus <- private$..corpus$addDocument(doc)  # Add document to corpus
      }

      private$..state <- paste0("Created corpus ", private$..meta[["name"]], " from a Quanteda corpus object.")
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
      visitor$corpusSourceQuanteda(self)
    }
  )
)
