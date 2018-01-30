#' @section Process Family of Classes Introduction:
#' The Process family of classes is an implementation of the
#' strategy design pattern as described in the book "Design Patterns:
#' Elements of Reusable Object-Oriented Software" by Erich Gamma, Richard
#' Helm, Ralph Johnson and John Vlissides (hence Gang of Four). This
#' strategy design pattern allows for the definition of separate
#' Process algorithms to be applied to Corpus and Document objects.
#'
#' \strong{Process Superclass and Subordinate Abstract Classes:}
#'  \itemize{
#'   \item Process0: Abstract base class that defines methods and interfaces common to  all Process classes.
#'   \item ProcessCorpus0: Abstract base class that defines methods and interfaces common to all Process classes which operate on Corpus objects.
#'   \item ProcessDocument0: Abstract base class that defines methods and interfaces common to all Process classes which operate on Document objects.
#'  }
#'
#' \strong{Concrete Corpus Process Classes:}
#'  \itemize{
#'   \item ProcessCorpusRepairCtrl: Concrete class responsible for repairing control character presentations in Corpus objects.
#'   \item ProcessCorpusRepairEncoding: Concrete class responsible for repairing non-UTF8 character presentations in Corpus objects.
#'   \item ProcessCorpusParsing: Concrete class responsible for parsing specified content types, such as URLs from text in Corpus objects.
#'   \item ProcessCorpusNormalize: Concrete class responsible for normalizing text in Corpus objects.
#'   \item ProcessCorpusFilter: Concrete class responsible for filtering specified text or substrings from Corpus objects.
#'  }
#'
#' \strong{Concrete Document Process Classes:}
#'  \itemize{
#'   \item ProcessDocumentRepairCtrl: Concrete class responsible for repairing control character presentations in Document objects.
#'   \item ProcessDocumentRepairEncoding: Concrete class responsible for repairing non-UTF8 character presentations in Document objects.
#'   \item ProcessDocumentParsing: Concrete class responsible for parsing specified content types, such as URLs from text in Document objects.
#'   \item ProcessDocumentNormalize: Concrete class responsible for normalizing text in Document objects.
#'   \item ProcessDocumentFilter: Concrete class responsible for filtering specified text or substrings from Document objects.
#'  }
