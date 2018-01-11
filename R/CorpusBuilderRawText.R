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

      private$..name <- name
      private$..path <- path
      private$..dataSource <- dataSource

      private$..className <- 'CorpusBuilderRawText'
      private$..methodName <- 'initialize'
      private$..state <- paste0("CorpusBuilderRawText object instantiated.")
      private$..logs <- LogR$new()

      # Validate CorpusBuilder
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      private$..corpus <- Corpus$new(name = name, path = path)

      # Create log entry
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                              Build Methods                              #
    #-------------------------------------------------------------------------#
    buildDocuments = function() {

      private$..methodName <- "buildDocuments"
      ds <- private$..dataSource$getSource()

      # Create document object and add meta data
      if (isDirectory(ds) == TRUE) {
        files <- list.files(ds, full.names = TRUE)
        private$..documents <- lapply(files, function(f) {
          # Obtain file meta data
          name <- tools::file_path_sans_ext(basename(f))
          Document$new(name)$load(f)$meta(key = "directory", value = private$..path)
        })
      } else if (class(ds) == "character") {
        private$..documents <- lapply(seq_along(ds), function(x) {
          name <- names(ds[x])
          if (is.null(name)) paste0("Document", x, collapse = "-")
          Document$new(name = name)$setContent(ds[x])
        })
      }

      # Logit
      private$..state <- paste0("Built raw documents for ", private$..name,
                                " corpus. ")
      self$logIt()

      invisible(self)
    },

    buildCorpus = function() {

      private$..methodName <- 'buildCorpus'

      lapply(private$..documents, function(d) {
        private$..corpus$addDocument(d)
      })

      # Logit
      private$..state <- paste0("Added raw documents to ", private$..name,
                                " corpus. ")
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
