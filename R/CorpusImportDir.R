#==============================================================================#
#                               CorpusImportDir                                #
#==============================================================================#
#' CorpusImportDir
#'
#' \code{CorpusImportDir} Creates Corpus object from directory sources.
#'
#' @template corpusImportStrategyClasses
#'
#' @section CorpusImportDir Methods:
#' @template corpusImportStrategyMethods
#'
#' @template corpusImportStrategyParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusBuilder family of classes
#' @export
CorpusImportDir <- R6::R6Class(
  classname = "CorpusImportDir",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CorpusImport0,

  public = list(

    #-------------------------------------------------------------------------#
    #                       Initialization Method                             #
    #-------------------------------------------------------------------------#
    initialize = function(name, dataSource) {

      private$..dataSource <- dataSource

      private$..className <- 'CorpusImportDir'
      private$..methodName <- 'initialize'
      private$..logs <- LogR$new()

      if (private$validateParams()$code == FALSE) stop()

      private$..corpus <- Corpus$new(name = name)

      # Create log entry
      private$..state <- paste0("Corpus Directory Import object instantiated")
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
      } else if ("character" %in% class(private$..dataSource)) {
        path <- private$..dataSource
        glob <- basename(path)
        dir <- dirname(path)
        files <- list.files(dir, pattern = glob2rx(glob), full.names = TRUE)
      } else {
        private$..state <- paste0("Unable to import corpus from ",
                                  private$..dataSource, ". ",
                                  "Invalid file path. See ?",
                                  class(self)[1], " for further assistance.")
        self$logIt("Error")
        stop()
      }

      if (length(files) != 0) {

        # Extract meta data
        fileNames <- basename(files)
        docNames <- tools::file_path_sans_ext(fileNames)
        source <- dirname(files)

        # Read content
        content <- lapply(files, function(f) {
          io <- IOFactory$new(f)$getIOStrategy()
          io$read(path = f)
        })

        # Create documents
        docs <- lapply(seq_along(content), function(x) {
          doc <- Document$new(name = docNames[x])
          doc$content <- content[[x]]
          doc$meta(key = "fileName", value = fileNames[x])
          doc$meta(key = "filePath", value = files[x])
          doc$meta(key = "source", value = source[x])
          doc
        })

        # Add documents to corpus
        for (i in 1:length(docs)) {
          private$..corpus <- private$..corpus$addDocument(docs[[i]])
        }

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
      visitor$corpusImportDir(self)
    }
  )
)
