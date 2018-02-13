#' @section TextStudio Class Overview:
#' The TextStudio class is a modification to the command design pattern,
#' as described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). The command pattern encapsulates
#' all information required to perform an action or trigger an event
#' for a later time. Rather than invoking a method on the receiver,
#' the Invoker accepts the receiver and the commands from the client.
#' When the "execute method" is invoked, the Invoker processes the
#' commands against the receiver, then returns the result to
#' the client upon completion of the commands.
#'
#' \strong{TextStudio Classes:}
#' \itemize{
#'  \item TextStudio: The class responsible for executing commands upon a Corpus object.
#'  \item TextCheck: The class responsible for auditing text, discovering problems and recommending solutions#'
#'  }
#'
