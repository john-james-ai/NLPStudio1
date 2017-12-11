#==============================================================================#
#                             CorpusDirector                                   #
#==============================================================================#
#' CorpusDirector
#'
#' \code{CorpusDirector} Class that directs the construction of various types of corpora.
#'
#' Class directs the construction of the raw, refined, reshaped and processed corpora
#'
#' The Document family of classes is an implementation of the builder
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows the
#' construction process to be defined at runtime.
#'
#' @section Corpus Builder Participants:
#'  \describe{
#'   \item{CorpusBuilder0}{This abstract builder interface. Defines the methods for concrete corpus builder sub-classes. }
#'   \item{CorpusBuilderCv}{Concrete builder sub-class that produces the a single training, validation, and test corpora. }
#'   \item{CorpusBuilderKFoldCv}{Concrete builder sub-class that produces the K-Fold cross validation corpora. }
#'   \item{CorpusDirector}{Class that builds the corpus through the concrete builder classes..}
#'   \item{Corpus}{The corpus product.}
#'   }
#'
#' @section CorpusDirector Methods:
#'  \describe{
#'   \item{\code{new(builder)}}{Instantiates the director object}
#'   \item{\code{construct()}}{Initiates the construction process for the builder object.}
#'  }
#'
#'
#' @param builder Builder object for the Corpus object
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Corpus builder family
#' @export
CorpusDirector <- R6::R6Class(
  classname = "CorpusDirector",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..builder = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                       Initialization Method                             #
    #-------------------------------------------------------------------------#
    initialize = function(builder)  {

      private$..builder <- builder
    },

    construct = function() {
      private$..builder()
    }
  )
)
