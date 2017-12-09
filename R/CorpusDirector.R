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
#'   \item{CorpusBuilder0}{The abstract builder interface. Defines the interface for concrete corpus builder sub-classes. }
#'   \item{CorpusBuilderRawWeb}{Concrete builder subclass that produces the Raw Corpus object. }
#'   \item{CorpusBuilderRefined}{Concrete builder subclass that produces the Refined Corpus object. }
#'   \item{CorpusBuilderReshaped}{Concrete builder subclass that produces the Reshaped Corpus object. }
#'   \item{CorpusBuilderProcessed}{Concrete builder subclass that produces the Processed Corpus object. }
#'   \item{CorpusDirector}{Class that builds the corpus through the concrete builder interfaces.}
#'   \item{Corpus}{The corpus product.}
#'   }
#'
#' @section CorpusDirector Methods:
#'  \describe{
#'   \item{\code{new(builder, parserCmd = NULL)}}{Instantiates the director object}
#'   \item{\code{construct()}}{Initiates the construction process for the builder object.}
#'  }
#'
#'
#' @param builder Builder object for the Corpus object
#' @param size Numeric proportion of total object size to retain for training. Validation and test sets retain 20 percent of the corpus each.
#' @param parserCmd Parser command object for parsing the data during Processing
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
