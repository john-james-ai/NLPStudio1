#==============================================================================#
#                             CorpusSource0                                    #
#==============================================================================#
#' CorpusSource0
#'
#' \code{CorpusSource0} Abstract class CorpusSource family of classes.
#'
#' Provides the basic interface and core methods for the CorpusSource
#' concrete classes.
#'
#' The CorpusSource0 family of classes is an implementation of the strategy
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows corpus
#' sourcing behavior to be defined at run time.
#'
#' @section CorpusSource0 Family Participants:
#'  \itemize{
#'   \item{CorpusSource0}{This abstract class that defines core methods.}
#'   \item{CorpusSourceWeb}{Class responsible for obtaining corpus data from web sources.}
#'  }
#'
#' @section CorpusSource0 method:
#'  \itemize{
#'   \item{\code{new(...)}}{Method not implemented for this abstract class.}
#'   \item{\code{sourceData()}}{Method not implemented for this abstract class.}
#'  }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusSource classes
#' @export
CorpusSource0 <- R6::R6Class(
  classname = "CorpusSource0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  public = list(

    initialize = function(...) { Stop("This method is not defined for this abstract class.") },
    sourceData = function() { Stop("This method is not defined for this abstract class.") }
  )
)
