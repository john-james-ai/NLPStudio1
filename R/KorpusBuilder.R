#==============================================================================#
#                             KorpusBuilder0                                   #
#==============================================================================#
#' KorpusBuilder0
#'
#' \code{KorpusBuilder0} Abstract class for the Korpus builder classes
#'
#' The Document family of classes is an implementation of the builder
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows the
#' construction process to be defined at runtime.
#'
#' @section Korpus Builder Participants:
#'  \describe{
#'   \item{KorpusBuilder0}{This abstract builder interface. Defines the interface for concrete corpus builder sub-classes. }
#'   \item{KorpusDirector}{Class that builds the corpus through the concrete builder interface.}
#'   \item{Korpus}{The corpus or corpus set product.}
#'   }
#'
#' @section Korpus Builder0 Methods:
#'  \describe{
#'   \item{\code{new()}}{Instantiates the builder object}
#'   \item{\code{getData()}}{Obtains the corpus data from an external source.}
#'   \item{\code{buildDocuments()}}{Builds document objects.}
#'   \item{\code{repairDocuments()}}{Rapairs corpus document objects}
#'   \item{\code{splitDocuments()}}{Creates random sample of 10 indices, each representing 10 pct of the lines of the document..}
#'   \item{\code{sampleDocuments()}}{Creates sammples of the document according to size increments of 10 pct.}
#'   \item{\code{normalizeDocuments()}}{Normalizes text in the document objects.}
#'   \item{\code{correctDocuments()}}{Corrects common misspellings, contractions, and abbreviations.}
#'   \item{\code{cleanLanguage()}}{Removes sentences containing profane language.}
#'   \item{\code{nGramDocuments()}}{Creates nGram representations of document objects.}
#'   \item{\code{posTagDocuments()}}{Creates POS tag representations of document objects.}
#'   \item{\code{deliverKorpus()}}{Returns Korpus object.}
#'  }
#'
#' @param getData Boolean, indicator to obtain the data from an external source. Default is TRUE
#' @param documents Boolean, indicates whether document objects are to be created. Default is TRUE
#' @param repair Boolean, indicates whether the documents should undergo repair. Default is TRUE
#' @param split Boolean, indicates whether documents splits should be created. Default is TRUE
#' @param sample Boolean, indicates whether document samples should be taken. Default is TRUE
#' @param normalize Boolean, indicates whether text normalization should take place.Default is TRUE
#' @param correct Boolean, to perform corrections. Default is TRUE
#' @param profanity Boolean, indicator to remove profanity. Default is TRUE
#' @param nGrams Numeric indicator of the number of nGrams to build
#' @param posTags Boolean, indicates whether to create POS tags.  Default is TRUE
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus build family
#' @export
KorpusBuilder <- R6::R6Class(
  classname = "KorpusBuilder",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = KorpusBuilder0,

  public = list(

    #-------------------------------------------------------------------------#
    #                         Korpus Instantiation                            #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL) {

      # Instantiate variables
      private$..className <- 'KorpusBuilder'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "corpus"), desc)
      private$..parent <- NLPStudio$new()$getInstance()
      private$..path <- file.path(NLPStudio$new()$getInstance()$getDirs()$korpora, private$..name)
      private$..dirs <- Constants$new()$getKorpusPaths()
      private$..state <- "Instantiated the Korpus Builder."
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..logs <- LogR$new(NLPStudio$new()$getInstance()$getDirs()$logs)

      # Create Directories
      lapply(private$..dirs, function(d) {
        oldw <- getOption("warn")
        options(warn = -1)
        dir.create(file.path(private$..path, d), recursive = TRUE)
        options(warn = oldw)
      })

      # Log it
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },
    getName = function() private$..name,

    #-------------------------------------------------------------------------#
    #                               Get Data                                  #
    #-------------------------------------------------------------------------#
    getData = function(compressed = TRUE, format = 'zip', listFiles = FALSE) {

      # Validation
      private$..methodName <- 'getData'
      v <- ValidatorKorpusBuilder$new()
      status <- v$getData(self)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = "Error")
        stop()
      }

      downloadPath <- file.path(private$..path, private$..dirs$external)
      fileName <- installr::file.name.from.url(private$..url)

      if (length(list.files(file.path(private$..path, private$..dirs$external))) < 1) {

        # Download data
        f <- FileManager$new()
        status <- f$download(private$..url, downloadPath)
        if (status[['code']] == FALSE) {
          private$..state <- status[['msg']]
          self$logIt(level = 'Error')
          stop()
        }
      }

      # Unzip adata
      f <- FileManager$new()
      if (compressed == TRUE) {
        if (format == 'zip') {
          oldw <- getOption("warn")
          options(warn = -1)
          status[['data']] <- f$unZipFile(zipFilePath = file.path(downloadPath,
                                                                  fileName),
                                          exDir = file.path(private$..path,
                                                            private$..dirs$raw),
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
        DocumentText$new(f)
      })
      private$..rawDocs <- documents
    },

    #-------------------------------------------------------------------------#
    #                           Test Methods                                  #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      #TODO: Remove after testing

      korpus = list(
        metaData = list(
          name = private$..name,
          desc = private$..desc,
          parent = private$..parent,
          path = private$..path,
          dirs = private$..dirs,
          logs = private$..logs,
          state = private$..state,
          modified = private$..modified,
          created = private$..created
        ),
        parameters = list(
          url = private$..url,
          files = private$..files,
          repairs = private$..repairs,
          splits = private$..splits,
          samples = private$..samples,
          normalize = private$..normalize,
          corrections = private$..corrections,
          profanity = private$..profanity
        ),
        documents = list(
          rawDocs = private$..rawDocs,
          preDocs = private$..preDocs,
          sets = private$..sets
        )
      )

      return(korpus)
    }
  )
)
