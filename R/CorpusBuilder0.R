#==============================================================================#
#                             CorpusBuilder0                                   #
#==============================================================================#
#' CorpusBuilder0
#'
#' \code{CorpusBuilder0} Abstract class for the CorpusBuilder classes
#'
#' The Document family of classes is an implementation of the builder
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
CorpusBuilder0 <- R6::R6Class(
  classname = "CorpusBuilder0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..name = character(),
    ..desc = character(),
    ..sourceCmd = character(),
    ..parseCmd = character(),
    ..cvCmd = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc, sourceCmd, parseCmd, cvCmd) stop("This method is not implemented in this abstract class."),
    buildRaw = function() stop("This method is not implemented in this abstract class."),
    buildRefined = function() stop("This method is not implemented in this abstract class."),
    buildReshaped = function() stop("This method is not implemented in this abstract class."),
    buildCV = function() stop("This method is not implemented in this abstract class."),
    buildParsed = function() stop("This method is not implemented in this abstract class."),
    buildNormalized = function() stop("This method is not implemented in this abstract class."),
    buildFiltered = function() stop("This method is not implemented in this abstract class.")
  )
)
