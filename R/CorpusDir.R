#==============================================================================#
#                                  CorpusDir                                   #
#==============================================================================#
#' CorpusDir
#'
#' \code{CorpusDir} Creates Corpus object from directory sources.
#'
#' @template corpusBuilderClasses
#'
#' @section CorpusDir Methods:
#' @template corpusBuilderMethods
#'
#' @template corpusBuilderParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusBuilder family of classes
#' @export
CorpusDir <- R6::R6Class(
  classname = "CorpusDir",
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

      private$..className <- 'CorpusDir'
      private$..methodName <- 'initialize'
      private$..state <- paste0("CorpusDir object instantiated.")
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

      # Extract file names
      if (isDirectory(private$..dataSource)) {
        files <- list.files(private$..dataSource, full.names = TRUE)
      } else if (class(private$..dataSource) == "character") {
        path <- private$..dataSource
        glob <- basename(path)
        dir <- dirname(path)
        files <- list.files(dir, pattern = glob2rx(glob), full.names = TRUE)
      }

      # Extract meta data
      fileNames <- basename(files)
      docNames <- tools::file_path_sans_ext(fileNames)
      source <- dirname(files)

      # Create documents
      docs <- lapply(seq_along(files), function(x) {
        doc <- Document$new(name = docNames[x])
        doc <- doc$load(path = files[x])
        doc$meta(key = "fileName", value = fileNames[x])
        doc$meta(key = "source", value = source[x])
        doc
      })

      # Add documents to corpus
      for (i in 1:length(docs)) {
        private$..corpus <- private$..corpus$addDocument(docs[[i]])
      }

      private$..state <- paste0("Created raw corpus ", private$..name, ".")
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
