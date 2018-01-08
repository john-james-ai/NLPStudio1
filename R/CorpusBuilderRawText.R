#==============================================================================#
#                             CorpusBuilderRawText                             #
#==============================================================================#
#' CorpusBuilderRawText
#'
#' \code{CorpusBuilderRawText} Concrete builder class for raw Corpus from text sources.
#'
#' @template corpusBuilderClasses
#'
#' @section CorpusBuilderRawText Methods:
#' @template corpusBuilderMethods
#'
#' @template corpusBuilderParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusBuilder family of classes
#' @export
CorpusBuilderRawText <- R6::R6Class(
  classname = "CorpusBuilderRawText",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CorpusBuilder0,

  public = list(

    #-------------------------------------------------------------------------#
    #                     Pipeline Initialization Method                      #
    #-------------------------------------------------------------------------#
    initialize = function(name, path, dataSource) {

      private$..corpus <- Corpus$new(name = name, path = path)
      private$..dataSource <- dataSource

      private$..className <- 'CorpusBuilderRawText'
      private$..methodName <- 'initialize'
      private$..state <- paste0("CorpusBuilderRawText object instantiated.")
      private$..created <- Sys.time()
      private$..modified <- Sys.time()
      private$..accessed <- Sys.time()
      private$..logs <- LogR$new()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                              Build Methods                              #
    #-------------------------------------------------------------------------#
    buildDocuments = function() {

      private$..methodName <- "buildDocuments"
      ds <- private$..dataSource$getDataSource()

      # Create document object and add meta data
      if (isDirectory(ds) == TRUE) {
        files <- list.files(ds, full.names = TRUE)
        data <- lapply(files, function(f) {
          # Obtain file meta data
          fileName <- basename(f)
          filePath <- f
          fileSize <- file.size(f)
          created <- file.info(f)[,'ctime']
          modified <- file.info(f)[,'mtime']
          created <- file.info(f)[,'ctime']
          io <- IOFactory$new()$getIOStrategy(f)
          txt <- io$read(f)
        })
      }
    },
    buildCorpus = function() { stop("This method is not implemented for this abstract class.") },
    getResult = function() { stop("This method is not implemented for this abstract class.") },

    #-------------------------------------------------------------------------#
    #                             Other Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  { stop("This method is not implemented for this abstract class. ") },
    logIt = function(level = 'Info') { stop("This method is not implemented for this abstract class. ") }
  )
)
