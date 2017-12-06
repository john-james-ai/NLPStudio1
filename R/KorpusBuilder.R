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
#' @section Korpus Builder Participants
#'  \describe{
#'   \item{KorpusBuilder0}{This abstract builder interface. Defines the interface for concrete corpus builder sub-classes. }
#'   \item{KorpusDirector}}{Class that builds the corpus through the concrete builder interface.}
#'   \item{Korpus}}{The corpus or corpus set product.}
#'
#' @section Korpus Builder0 Methods:
#'  \describe{
#'   \item{\code{new()}}{Instantiates the builder object}
#'   \item{\code{getData()}}{Obtains the corpus data from an external source.}
#'   \item{\code{buildDocuments()}}{Builds document objects.}
#'   \item{\code{repairDocuments()}}{Rapairs corpus document objects}
#'   \item{\code{splitDocuments()}}{Creates random sample of 10 indices, each representing 10% of the lines of the document..}
#'   \item{\code{sampleDocuments()}}{Creates sammples of the document according to size increments of 10%.}
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
      private$..path <- NLPStudio$new()$getInstance()$getDirs()$korpora
      private$..dirs <- Constants$new()$getKorpusPaths()
      private$..logs <- NLPStudio$new()$getInstance()$getDirs()$logs
      private$..state <- "Instantiated the Korpus Builder."
      private$..modified <- Sys.time()
      private$..created <- Sys.time()

      # Log it
      private$..logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                      Korpus Build Methods                               #
    #-------------------------------------------------------------------------#
    getData = function() {

      v <- Validator$new()
      v$getData(self)

      downloadPath <- file.path(private$..path, private$..extDir)
      fileName <- installr::file.name.from.url(private$..url)

      f <- FileManager$new()
      status <- f$download(private$..url, downloadPath)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        private$..logIt(level = 'Error')
        stop()
      }

      if (compressed == TRUE) {
        if (format == 'zip') {
          status[['data']] <- f$unZipFile(zipFilePath = file.path(downloadPath, fileName),
                                          exDir = file.path(private$..path, private$..extDir),
                                          files = private$..files, list = listFiles)
          if (status[['code']] == FALSE) {
            private$..state <- status[['msg']]
            private$..logIt(level = 'Error')
          }
        }
      }
      invisible(self)
    },

    getDocuments = function() {

    }




    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$korpus(self)
    },

    #-------------------------------------------------------------------------#
    #                            Log Method                                   #
    #-------------------------------------------------------------------------#
    logIt = function(level = 'Info', fieldName = NA) {

      private$..logs$entry$owner <- private$..name
      private$..logs$entry$className <- private$..className
      private$..logs$entry$methodName <- private$..methodName
      private$..logs$entry$level <- level
      private$..logs$entry$msg <- private$..state
      private$..logs$entry$fieldName <- fieldName
      private$..logs$created <- Sys.time()
      private$..logs$writeLog()
    },

    #-------------------------------------------------------------------------#
    #                           Test Methods                                  #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      #TODO: Remove after testing

      Builder0 = list(
        className = private$..className,
        methodName = private$..methodName,
        name = private$..name,
        builder = private$..builder,
        state = private$..state,
        modified = private$..modified,
        created = private$..created
      )

      return(Builder0)
    }
  )
)
