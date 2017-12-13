#==============================================================================#J
#                             SourceData0                                      #
#==============================================================================#
#' SourceData0
#'
#' \code{SourceData0} Abstract class defining methods for SourceData family of classes.
#'
#' The SourceData0 family of classes is an implementation of the strategy
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows corpus
#' sourcing behavior to be defined at run time.
#'
#' @section SourceData Family Participants:
#'  \itemize{
#'   \item{SourceData0}{Abstract class that defines core methods.}
#'   \item{SourceDataTxt}{Class responsible for obtaining corpus data from text files.}
#'   \item{SourceDataCsv}{Class responsible for obtaining corpus data from csv files.}
#'   \item{SourceDataRdata}{Class responsible for obtaining corpus data from Rdata files.}
#'   \item{SourceDataRds}{Class responsible for obtaining corpus data from Rds files.}
#'   \item{SourceDataJson}{Class responsible for obtaining corpus data from JSON files.}
#'   \item{SourceDataXml}{Class responsible for obtaining corpus data from XML files.}
#'  }
#'
#' @section SourceData0 methods:
#'  \itemize{
#'   \item{\code{new(...)}}{Instantiates a SourceData object. Method not implemented for this abstract class.}
#'   \item{\code{sourceData()}}{Sources the data and creates a Corpus object. Method not implemented for this abstract class.}
#'   \item{\code{accept()}}{Accepts a Visitor object. Method not implemented for this abstract class. }
#'   \item{\code{logIt()}}{Method used for logging purposes. Method not implemented for this abstract class. }
#'  }
#'
#' @param name Character string indicating the name of the SourceData0 object.
#' @param desc Character string containing the description of the corpus.
#' @param path Character string indicating the directory
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusSource classes
#' @export
SourceData0 <- R6::R6Class(
  classname = "SourceData0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Corpus0,

  private = list(

    ..pattern = character(),

    listFiles = function() {

      if (isDirectory(private$..pattern)) {
        files <- list.files(private$..pattern, full.names = TRUE)
      } else {
        dirName <- dirname(private$..pattern)
        wildcard <- basename(private$..pattern)
        files <- list.files(dirName, pattern = glob2rx(wildcard), full.names = TRUE)
      }
      return(files)
    },

    validateSource = function() {

      status <- list()
      status[['code']] <- TRUE
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = "Error")
        stop()
      }
    },

    loadData = function() {
      lapply(private$..documents, function(d) {
        d$loadFile
      })
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                        Source Core Methods                              #
    #-------------------------------------------------------------------------#
    initialize = function(name, pattern) { stop("This method is not implemented for this abstract class.") },
    sourceData = function() {stop("This method is not implemented for this abstract class.")},

    #-------------------------------------------------------------------------#
    #                           Expose Object                                 #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      o <- list(
        className	 =  private$..className,
        methodName = private$..methodName,
        name = private$..name,
        pattern = private$..pattern,
        documents = private$.documents,
        state = private$..state,
        logs = private$..logs,
        modified = private$..modified,
        created = private$..created,
        accessed = private$..accessed
      )
      return(o)
    }

  )
)
