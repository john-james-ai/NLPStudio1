#==============================================================================#
#                               BuildData                                      #
#==============================================================================#
#' BuildData
#'
#' \code{BuildData} Class responsible for building the processed data sets.
#'
#' This class obtains the corpus from external sources, creates the Corpus and
#' Document class objects, performs text parsing, normalization, and filtering.
#'
#' The Document family of classes is an implementation of the builder
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows the
#' construction process to be defined at runtime. The end product of
#' the Builder process is the Data Analytics Platform (DAP).
#'
#'
#' @section Builder0 Participants:
#'  \describe{
#'   \item{Builder0}{The abstract builder class, which defines a common interface and methods for the Builder family of classes.}
#'   \item{BuildData}{Class responsible for sourcing and initializing the Corpus and Document objects.}
#'   \item{BuildFeatures}{Class responsible for feature engineering, such as NGrams, POS tags, and TDM.}
#'   \item{BuildModelMKN}{Class responsible for building the MKN Language Model.}
#'   \item{BuildModelKatz}{Class responsible for building the Katz Language Model.}
#'   }
#'
#' @section Build Document Methods:
#'  \describe{
#'   \item{\code{new()}}{Instantiates the builder object.}
#'   \item{\code{getData()}}{Obtains the corpus data from an external source.}
#'   \item{\code{BuildData()}}{Builds the document objects objects.}
#'   \item{\code{encodeDocuments()}}{Corrects file encoding.}
#'  }
#'
#' @param name Character string including the name of the Corpus object
#' @param desc Character string including a description for the Corpus object
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus Builder
#' @export
BuildData <- R6::R6Class(
  classname = "BuildData",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Builder0,

  private = list(
    ..url = character(),
    ..files = character(),
    ..rawDocs = list(),
    ..preDocs = list(),
    ..proDocs = list(),
    ..dirs = list(
      extDir = 'data/external',
      rawDir = 'data/raw',
      preDir = 'data/preprocessed',
      proDir = 'data/processed'
    )
  ),

  active = list(
    url = function(value) {
      if (missing(value)) {
        private$..url
      } else {
        private$..url <- url
      }
    },

    files = function(value) {
      if (missing(value)) {
        private$..files
      } else {
        private$..files <- files
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Instantiation                               #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL) {

      # Instantiate variables
      private$..className <- 'CorpusBuilder'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "corpus"), desc)
      private$..parent <- NLPStudios$new()$getInstance()
      private$..path <- file.path(NLPStudios$new()$getInstance()$getDirs()$corpora, private$..name)
      private$..state <- "Instantiated the Corpus Builder."
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..logs <- LogR$new()

      # Log it
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                               Get Data                                  #
    #-------------------------------------------------------------------------#
    getData = function(compressed = TRUE, format = 'zip', listFiles = FALSE) {

      # Validation
      private$..methodName <- 'getData'
      v <- ValidatorBuilder$new()
      status <- v$getData(self)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = "Error")
        stop()
      }

      downloadPath <- file.path(private$..path, private$..dirs$extDir)
      fileName <- installr::file.name.from.url(private$..url)
      dir.create(downloadPath,  showWarnings = FALSE, recursive = TRUE)

      if (length(list.files(file.path(private$..path, private$..dirs$extDir))) < 1) {

        # Download data
        f <- FileManager$new()
        status <- f$download(private$..url, downloadPath)
        if (status[['code']] == FALSE) {
          private$..state <- status[['msg']]
          self$logIt(level = 'Error')
          stop()
        }
      }

      # Unzip data
      f <- FileManager$new()
      if (compressed == TRUE) {
        if (format == 'zip') {
          oldw <- getOption("warn")
          options(warn = -1)
          status[['data']] <- f$unZipFile(zipFilePath = file.path(downloadPath,
                                                                  fileName),
                                          exDir = file.path(private$..path,
                                                            private$..dirs$rawDir),
                                          files = private$..files, list = listFiles,
                                          overwrite = FALSE)
          options(warn = oldw)

          if (status[['code']] == FALSE) {
            private$..state <- status[['msg']]
            self$logIt(level = 'Error')
            stop()
          }
        }
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                         Build Documents                                 #
    #-------------------------------------------------------------------------#
    buildDocuments = function() {
      files <- list.files(path = file.path(private$..path, private$..dirs$raw),
                          full.names = TRUE)
      documents <- lapply(files, function(f) {
        Document$new(f)
      })
      private$..rawDocs <- documents
    },

    #-------------------------------------------------------------------------#
    #                              Parsing                                    #
    #-------------------------------------------------------------------------#









    #-------------------------------------------------------------------------#
    #                         Deliver Product                                 #
    #-------------------------------------------------------------------------#
    deliver = function() {
      Corpus$new(name = private$..name, desc = private$..desc,
                 extDocs = private$..extDocs, rawDocs = private$..rawDocs,
                 preDocs = private$..preDocs)
    },


    #-------------------------------------------------------------------------#
    #                           Test Methods                                  #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      #TODO: Remove after testing

      corpus = list(
        name = private$..name,
        desc = private$..desc,
        parent = private$..parent,
        path = private$..path,
        dirs = private$..dirs,
        logs = private$..logs,
        state = private$..state,
        modified = private$..modified,
        created = private$..created,
        url = private$..url,
        files = private$..files,
        rawDocs = private$..rawDocs,
        preDocs = private$..preDocs,
        proDocs = private$..proDocs
        )
      return(corpus)
    }
  )
)
