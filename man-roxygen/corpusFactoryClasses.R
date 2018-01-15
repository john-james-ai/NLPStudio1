#' @section CorpusFactory Family of Classes Introduction:
#' The CorpusFactory family of classes is an implementation of the
#' factory method design pattern, as described in the book "Design Patterns:
#' Elements of Reusable Object-Oriented Software" by Erich Gamma, Richard
#' Helm, Ralph Johnson and John Vlissides (hence Gang of Four). This
#' factory method design pattern allows for the definition of
#' separate methods for repairing, reshaping, splitting,
#' and preprocessing the data.
#'
#' \strong{Corpus Factory Classes:}
#' \itemize{
#'  \item Corpus: Class that contains the collection of Document objects.
#'  \item CorpusFactory0: Abstract class that defines the interface for the concrete factory classes
#'  \item CorpusRepair: Class the repairs ASCII control character encoding
#'  \item CorpusReshape: Class that reshapes the data into one sentence per vector.
#'  \item CorpusSplit: Class responsible for creating hold-out and k-fold cross validation sets
#'  \item CorpusPreprocess: Class responsible for preprocessing the corpus
#'  }
#'
