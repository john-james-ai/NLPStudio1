#==============================================================================#
#                             CorpusBuilderRaw                             #
#==============================================================================#
#' CorpusBuilderRaw
#'
#' \code{CorpusBuilderRaw} Concrete builder class for raw Corpus from text sources.
#'
#' @template corpusBuilderClasses
#'
#' @section CorpusBuilderRaw Methods:
#' @template corpusBuilderMethods
#'
#' @template corpusBuilderParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusBuilder family of classes
#' @export
CorpusBuilderRaw <- R6::R6Class(
  classname = "CorpusBuilderRaw",
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

      private$..className <- 'CorpusBuilderRaw'
      private$..methodName <- 'initialize'
      private$..state <- paste0("CorpusBuilderRaw object instantiated.")
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
    build = function() {

      private$..methodName <- "buildDocuments"

      docs <- private$..dataSource

      if (class(docs) == "character") {

        # Format metadata
        name <- docNames[x]
        fileName <- paste0(name, ".txt")
        directory <- private$..path

        doc <- Document$new(name = private$..name)
        doc$content <- docs
        doc <- doc$meta(key = "name", value = name)
        doc <- doc$meta(key = "fileName", value = fileName)
        doc <- doc$meta(key = "directory", value = directory)

        # Save the document to file
        path <- file.path(private$..path, fileName)
        doc$save(path)
        private$..documents <- list(doc)

      } else if (class(docs) == "list") {

        if (is.null(names(docs))) {
          docNames <- paste0("Document", seq_along(docs))
        } else {
          docNames <- names(docs)
        }

        private$..documents <- lapply(seq_along(docs), function(x) {
          # Format metadata
          name <- docNames[x]
          fileName <- paste0(name, ".txt")
          directory <- private$..path

          # Instantiate document and format metadata
          doc <- Document$new(name = name)
          doc$content <- docs[[x]]
          doc <- doc$meta(key = "name", value = name)
          doc <- doc$meta(key = "fileName", value = fileName)
          doc <- doc$meta(key = "directory", value = directory)

          # Save the document to file
          path <- file.path(private$..path, fileName)
          doc$save(path)
          doc
        })
      } else {
        private$..state <- paste0("Unable to build corpus. Invalid ",
                                  "data source. See?", class(self),
                                  " for further assistance.")
        self$logIt("Error")
        stop()
      }

      for (i in 1:length(private$..documents)) {
        private$..corpus <- private$..corpus$addDocument(private$..documents[[i]])
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
