#' @section Source Family of Classes Introduction:
#' The Source family of classes is an implementation of the
#' strategy design pattern, as described in the book "Design Patterns: Elements
#'  of Reusable Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This strategy pattern
#' allows specific Corpus object import algorithms to be designated
#' at run time based upon the source of the data.
#'
#' \strong{Import Classes:}
#' \itemize{
#'  \item Corpus: Class that contains the collection of Document objects.
#'  \item CorpusCSV: Class creates a Corpus object from a CSV source.
#'  \item SourceDir: Class creates a Corpus object from files contained a designated directory .
#'  \item CorpusJSON: Class creates a Corpus object from a JSON source.
#'  \item SourceVector: Class creates Corpus objects from text sources.
#'  \item CorpusXML: Class creates a Corpus object from an XML source.
#'  \item SourceQuanteda: Class creates a Corpus object from an Quanteda corpus object.
#'  \item CorpusTM: Class creates a Corpus object from an TM VCorpus object.
#'  \item CorpusKoRpus: Class creates a Corpus object from an KoRpus package corpus object.
#'  }
#'
