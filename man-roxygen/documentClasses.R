#' @section Document Family of Classes:
#' The Document family of classes is an implementation of the composite design pattern,
#' as described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This composite pattern allows
#' clients to treat Corpus and Document objects uniformly,
#'
#' \strong{Document Family of Classes Participants:}
#' \itemize{
#'  \item Document0: Abstract component class which declares the interface for
#'  the objects in the composition, as well as base methods shared by the
#'  Document and Corpus sub-classes.
#'  \item Document: Class the represents the data and behaviors of
#'  individual document objects.
#'  \item Corpus: Composite collection of Document objects.
#'  }
#'
