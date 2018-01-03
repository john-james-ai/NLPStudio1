#==============================================================================#
#                           CorpusImportFC                                     #
#==============================================================================#
#' CorpusImportFC
#'
#' \code{CorpusImportFC} Class responsible for importing a FileCollection object into a Corpus object.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(x)}}{Instantiates the CorpusImportFC object.}
#'  \item{\code{execute()}}{Executes the corpus import.}
#' }
#'
#' @param x FileCollection object
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusImport classes
#' @export
CorpusImportFC <- R6::R6Class(
  "CorpusImportFC",
  inherit = CorpusImport0,

  public = list(
    initialize = function(x) {

      # Instantiate variables
      private$..name <- "corpusImportFC"
      private$..admin$className <- 'CorpusImportFC'
      private$..admin$methodName <- 'initialize'
      private$..admin$state <- paste0("Corpus, ", private$..name, ", instantiated.")
      private$..admin$modified <- Sys.time()
      private$..admin$created <- Sys.time()
      private$..admin$accessed <- Sys.time()
      private$..admin$logs <- LogR$new()

      if (class(x)[[1]] != "FileCollection") {
        private$..admin$state <- paste0("Parameter 'x' is invalid. Must be of the ",
                                  "FileCollection class.  ")
        self$logIt('Error')
        stop()
      }

      private$..source <- x
      invisible(self)
    },

    execute = function() {

      files <- private$..source$getFiles()

      # Create documents
      documents <- lapply(files, function(f) {
        name <- f$getName()
        Document$new(name = name, file = f)
      })
      return(documents)
    }
  )
)
