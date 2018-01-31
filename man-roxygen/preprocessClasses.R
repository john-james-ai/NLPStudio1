#' @section PrePreprocess Family of Classes Introduction:
#' The PrePreprocess Family of classes is an implementation of the
#' strategy design pattern as described in the book "Design Patterns:
#' Elements of Reusable Object-Oriented Software" by Erich Gamma, Richard
#' Helm, Ralph Johnson and John Vlissides (hence Gang of Four). This
#' strategy design pattern allows for the definition of separate
#' Preprocess algorithms to be applied to Corpus and Document objects.
#'
#' \strong{Preprocess Superclass and Subordinate Abstract Classes:}
#'  \itemize{
#'   \item Preprocess0: Abstract base class that defines methods and interfaces common to  all Preprocess classes.
#'   \item PreprocessCorpus0: Abstract base class that defines methods and interfaces common to all Preprocess classes which operate on Corpus objects.
#'   \item PreprocessDocument0: Abstract base class that defines methods and interfaces common to all Preprocess classes which operate on Document objects.
#'  }
#'
#' \strong{Concrete Corpus Preprocess Classes:}
#'  \itemize{
#'   \item PreprocessCorpusRepairCtrl: Concrete class responsible for repairing control character presentations in Corpus objects.
#'   \item PreprocessCorpusRepairEncoding: Concrete class responsible for repairing non-UTF8 character presentations in Corpus objects.
#'   \item PreprocessCorpusParsing: Concrete class responsible for parsing specified content types, such as URLs from text in Corpus objects.
#'   \item PreprocessCorpusNormalize: Concrete class responsible for normalizing text in Corpus objects.
#'   \item PreprocessCorpusFilter: Concrete class responsible for filtering specified text or substrings from Corpus objects.
#'  }
#'
#' \strong{Concrete Document Preprocess Classes:}
#'  \itemize{
#'   \item PreprocessDocumentRepairCtrl: Concrete class responsible for repairing control character presentations in Document objects.
#'   \item PreprocessDocumentRepairEncoding: Concrete class responsible for repairing non-UTF8 character presentations in Document objects.
#'   \item PreprocessDocumentParsing: Concrete class responsible for parsing specified content types, such as URLs from text in Document objects.
#'   \item PreprocessDocumentNormalize: Concrete class responsible for normalizing text in Document objects.
#'   \item PreprocessDocumentFilter: Concrete class responsible for filtering specified text or substrings from Document objects.
#'  }
