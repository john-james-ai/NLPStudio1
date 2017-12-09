#==============================================================================#
#                             CorpusSource                                     #
#==============================================================================#
#' CorpusSource
#'
#' \code{CorpusSource} Class responsible for invoking corpus sourcing commands.
#'
#' The CorpusSource family of classes is an implementation of the command
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows one to
#' construct general components that need to delegate, sequence or execute
#' method calls at run time without the need to know the
#' class of the method or the method parameters.
#'
#' @section CorpusSource Family Participants:
#'  \itemize{
#'   \item{Command0}{Abstract class that defines the Commmand interface.}
#'   \item{CorpusSource}{Invoker class that initiates the execution of a source command. }
#'   \item{Corpus}{Receiver class with the sourcing methods invoked by the CorpusSource.}
#'   \item{CorpusSourceWebCmd}{Concrete Command0 class for sourcing a corpus from the web.}
#'   \item{CorpusSourceLocal}{Concrete Command0 class for sourcing a corpus from another local corpus.}
#'  }
#'
#' @section CorpusSource method:
#'  \itemize{
#'   \item{\code{new(cmd)}}{Method invoking the web corpus source command.}
#'  }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusSource classes
#' @export
CorpusSource <- R6::R6Class(
  classname = "CorpusSource",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Command0,

  public = list(

    initialize = function(cmd) {
      cmd$execute()
    }
  )
)
