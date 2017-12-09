#==============================================================================#
#                           ParseLongWordCmd                                   #
#==============================================================================#
#' ParseLongWordCmd
#'
#' \code{ParseLongWordCmd} Concrete command class responsible for executing the email parse command.
#'
#' The Parser family of classes is an implementation of the command
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows one to
#' construct general components that need to delegate, sequence or execute
#' method calls at run time without the need to know the
#' class of the method or the method parameters.
#'
#' @section Parser Family Participants:
#'  \itemize{
#'   \item{Command0}{Abstract class that defines the Commmand interface.}
#'   \item{Parser}{Invoker class that initiates the execution of a parse command. }
#'   \item{Document}{Receiver class with the parsing methods invoked by the Parser.}
#'   \item{ParseEmailCmd}{Concrete Command0 class for parsing email addresses from a Document object.}
#'   \item{ParseUrlCmd}{Concrete Command0 class for parsing URLs addresses from a Document object.}
#'   \item{ParseTwitterCmd}{Concrete Command0 class for parsing twitter handles  from a Document object.}
#'   \item{ParseControlCmd}{Concrete Command0 class for parsing control characters  from a Document object.}
#'   \item{ParseHyphenCmd}{Concrete Command0 class for parsing hyphens  from a Document object.}
#'   \item{ParseAposCmd}{Concrete Command0 class for parsing apostrophes  from a Document object.}
#'   \item{ParsePunctCmd}{Concrete Command0 class for parsing punctuation  from a Document object.}
#'   \item{ParseSymbolCmd}{Concrete Command0 class for parsing symbols  from a Document object.}
#'   \item{ParseDigitCmd}{Concrete Command0 class for parsing digits  from a Document object.}
#'   \item{ParseRepeatCmd}{Concrete Command0 class for parsing repeated chars  from a Document object.}
#'   \item{ParseLongWordCmd}{Concrete Command0 class for parsing long words  from a Document object.}#'
#'  }
#'
#' @section ParseLongWordCmd methods:
#'  \itemize{
#'   \item{\code{initialize(set)}}{Accepts a Set Object containing Document receiver objects.}
#'   \item{\code{execute()}}{Iterates through a Set objects Document objects and executes the request.}
#'  }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Parser classes
#' @export
ParseLongWordCmd <- R6::R6Class(
  classname = "ParseLongWordCmd",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Command0,

  private = list(
    ..set = character()
  ),

  public = list(

    initialize = function(set)  {
      private$..set <- set
      invisible(self)
    },

    execute = function() {
      documents <- private$..set$getDocuments()
      lapply(documents, function(d) {
        d$parseLongWord()
      })
    }


  )
)
