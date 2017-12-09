#==============================================================================#
#                             CorpusBuilderCV                                  #
#==============================================================================#
#' CorpusBuilderCV
#'
#' \code{CorpusBuilderCV} Concrete class for building the traditional training, validation and test cross-validation sets.
#'
#' Class sources, repairs, and processes training, validation and test set corpora.
#'
#' The CorpusBuilder family of classes is an implementation of the builder
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows the
#' construction process to be defined at runtime.
#'
#' @section Corpus Builder Participants:
#'  \describe{
#'   \item{CorpusBuilder0}{This abstract builder interface. Defines the interface for concrete corpus builder sub-classes. }
#'   \item{CorpusBuilderCv}{Concrete builder sub-class that produces the a single training, validation, and test sets. }
#'   \item{CorpusBuilderKFoldCv}{Concrete builder sub-class that produces the K-Fold cross validation sets. }
#'   \item{CorpusDirector}{Class that builds the corpus through the concrete builder interfaces.}
#'   \item{Corpus}{The corpus product.}
#'   }
#'
#' @section Corpus CorpusBuilder0 Methods:
#'  \describe{
#'   \item{\code{new(name, desc, sourceCmd, parseCmd, cvCmd)}}{Instantiates the builder object}
#'   \item{\code{buildRaw()}}{Obtains and stores the raw data.}
#'   \item{\code{buildRefined()}}{Builds the refined data sets with encoding errors corrected.}
#'   \item{\code{buildReshaped()}}{Builds a corpus reshaped into sentences.}
#'   \item{\code{buildCV()}}{Builds the cross validation sets.}
#'   \item{\code{buildParsed()}}{Builds the parsed data sets.}
#'   \item{\code{buildNormalized()}}{Builds the normalized data sets.}
#'   \item{\code{buildFiltered()}}{Builds the filtered data sets.}
#'  }
#'
#' @param name Character string indicating the name of the Corpus object
#' @param desc Character string containing the description of the Corpus
#' @param sourceCmd Command object containing the commands for sourcing the corpus.
#' @param parseCmd Command object containing the commands for parsing the corpus.
#' @param cvCmd Command object containing the commands for building the cross-validation sets.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus builder classes
#' @export
CorpusBuilderCV <- R6::R6Class(
  classname = "CorpusBuilderCV",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = CorpusBuilder0,

  public = list(

    #-------------------------------------------------------------------------#
    #                       Initialization Method                             #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc, sourceCmd, parseCmd, cvCmd)  {

      private$..corpus <- Corpus$new(name, desc)
      private$..input <- url
      private$..files <- files
      private$..format <- format
      private$..path <- private$..corpus$getPath()
    },

    getData = function(compressed = TRUE, listFiles = FALSE) {

      # Validation
      private$..methodName <- 'getData'
      v <- Validator$new()
      status <- v$getData(self)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = "Error")
        stop()
      }

      # Obtain path and file name variables
      downloadPath <- file.path(private$..path, private$..extDir)
      fileName <- installr::file.name.from.url(private$..input)

      dir.create(downloadPath,  showWarnings = FALSE, recursive = TRUE)

      if (length(list.files(downloadPath)) < 1) {

        # Download data
        f <- FileManager$new()
        status <- f$download(private$..input, downloadPath)
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
                                          exDir = file.path(private$..path, private$..rawDir),
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

    buildDocuments = function() {
      dir <- file.path(private$..path, private$..rawDir)
      files <- list.files(dir, full.names = TRUE)
      documents <- lapply(files, function(f) {
        desc <- basename(f)
        desc <- tools::file_path_sans_ext(desc)
        Document$new(f, desc = desc)
      })
      return(documents)
    },

    buildCorpus = function(documents) {
      lapply(documents, function(d) {
        private$..corpus$addDocuments(d)
      })
      return(private$..corpus)
    },

    deliver = function() {
      return(private$..corpus)
    }
  )
)
