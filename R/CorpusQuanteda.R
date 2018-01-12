#==============================================================================#
#                                  CorpusQuanteda                              #
#==============================================================================#
#' CorpusQuanteda
#'
#' \code{CorpusQuanteda} Creates Corpus objects from Quanteda corpus objects.
#'
#' @template corpusBuilderClasses
#'
#' @section CorpusQuanteda Methods:
#' @template corpusBuilderMethods
#'
#' @template corpusBuilderParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusBuilder family of classes
#' @export
CorpusQuanteda <- R6::R6Class(
  classname = "CorpusQuanteda",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CorpusBuilder0,

  public = list(

    #-------------------------------------------------------------------------#
    #                       Initialization Method                             #
    #-------------------------------------------------------------------------#
    initialize = function(name, dataSource) {

      private$..name <- name
      private$..dataSource <- dataSource

      private$..className <- 'CorpusQuanteda'
      private$..methodName <- 'initialize'
      private$..state <- paste0("CorpusQuanteda object instantiated.")
      private$..logs <- LogR$new()

      # Create log entry
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                              Build Methods                              #
    #-------------------------------------------------------------------------#
    build = function() {

      private$..methodName <- "build"

      # Extract data
      texts <- private$..dataSource$documents[["texts"]]
      metaData <- private$..dataSource$documents[-which(names(private$..dataSource$documents) == "texts")]
      metaData$fileName <- rownames(metaData)

      # Create corpus
      corpus <-  CorpusText$new(name = private$..name, dataSource = texts)$build()$getResult()

      # Add metadata
      for (i in 1:length(metaData)) {
        corpus <- corpus$docMeta(key = names(metaData)[i], value = as.vector(metaData[,i]))
      }

      private$..corpus <- corpus

      private$..state <- paste0("Created corpus ", private$..name, " from a Quanteda corpus object.")
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
