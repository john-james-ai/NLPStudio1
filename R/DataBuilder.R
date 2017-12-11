#==============================================================================#
#                               DataBuilder                                    #
#==============================================================================#
#' DataBuilder
#'
#' \code{DataBuilder} Concrete DataBuilder class
#'
#' Class containing the methods for building the corpora, from sourcing, through
#' cross-validation splitting and preprocessing.
#'
#' The Data Builder family of classes is an implementation of the builder and
#' command patterns documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows the
#' construction process to be defined at runtime.
#'
#' @section Data Builder Participants:
#'  \describe{
#'   \item{DataBuilder0}{This abstract builder interface. Defines the methods for building the corpora. }
#'   \item{DataBuilder}{This concrete builder sub-class that produces the corpora. }
#'   \item{DataDirector}{Class that builds the corpora through the concrete builder classes.}
#'   \item{Data}{The data product, comprised of raw, refined, cross validation, and preprocessed corpora.}
#'   }
#'
#' @section Data DataBuilder Methods:
#'  \describe{
#'   \item{\code{new()}}{Instantiates the builder object}
#'   \item{\code{sourceData(url)}}{Sources corpus data from the web.}
#'   \item{\code{refine()}}{Corrects encoding errors}
#'   \item{\code{buildCV(train, val, test)}}{Splits data according to the parameters.}
#'   \item{\code{buildCVKFold(k)}}{Builds the K-Fold data sets for each register in the corpus.}
#'   \item{\code{parse(...)}}{Parses the data according to the parameter set.}
#'   \item{\code{normalize(norms)}}{Normalizes data according to the key/value data frame parameter.}
#'   \item{\code{filterWord(stopWords)}}{Removes the stopWords from the corpus}
#'   \item{\code{filterSent(stopWords)}}{Removes the sentences containing the stopWords from the corpus.}
#'   \item{\code{filterTag(stopWords, Tag)}}{Replaces stopwords with the tag parameter. }
#'   \item{\code{commit()}}{Commit preprocessing changes.}
#'  }
#'
#' @param name Character string indicating the name of the Data object
#' @param desc Character string containing the description of the Data
#' @param url Character string containing the URL from which the raw corpus data will be obtained.
#' @param train Numeric indicating the proportion of the data to retain for training
#' @param val Numeric indicating the proportion of the data to retain for validation
#' @param test Numeric indicating the proportion of the data to retain for testing
#' @param k Numeric indicating the number of folds to use in k-fold cross validation.
#' @param ... Parameters passed to the parsing method.
#' @param norms Data set containing key/value pairs of patterns and their replacements
#' @param stopWords Data set with a single column containing words to be removed or replaced in the corpus.
#' @param tag Character string containing the string with which to replace the stop words.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Data builder classes
#' @export
DataBuilder <- R6::R6Class(
  classname = "DataBuilder",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = DataBuilder0,

  private = list(
    ..data = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL) {

      data <- Data$new(name = name, desc = desc)

    },
    sourceData = function(url) {},
    refine = function() {},
    buildCV = function(train, val, test) {},
    buildCVKFold = function(k) {},
    parse = function(...) {},
    normalize = function(norms) {},
    filterWord = function(stopWords) {},
    filterSent = function(stopWords) {},
    filterTag = function(stopWords, tag) {},
    commit = function() {},

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$dataBuilder(self)
    },

    #-------------------------------------------------------------------------#
    #                           Test Methods                                  #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      #TODO: Remove after testing

      builder = list(
        data = private$..date
      )
      return(data)
    }

  )
)
