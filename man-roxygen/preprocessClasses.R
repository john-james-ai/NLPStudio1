#' @section Prepreprocess Family of Classes Introduction:
#' The Prepreprocess Family of classes is an implementation of the
#' Strategy and Factory Method design patterns as described in
#' the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard
#' Helm, Ralph Johnson and John Vlissides (hence Gang of Four). This
#' strategy design pattern allows for the definition of separate
#' Preprocess algorithms to be applied to Corpus and Document objects.
#'
#' \strong{Preprocess Core Classes:}
#'  \itemize{
#'   \item PreprocessBin: Class responsible for performing preprocessing of text files in binary format.
#'   \item PreprocessEncode: Class responsible for correcting common encoding errors.
#'   \item PreprocessReshape: Class responsible for reshaping the text into tokens, sentences, and/or paragraphs.
#'   \item PreprocessSplit: Class responsible for splitting a Corpus or Document for cross-validation purposes.
#'   \item PreprocessParse: Class responsible for parsing substrings from Corpus or Document objects.
#'   \item PreprocessNormalize: Class responsible for performing normalization tasks, such as contraction handling, on Corpus or Document objects.
#'   \item PreprocessFilter: Class responsible for filtering content from Corpus or Document objects.
#'  }
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
#'   \item PreprocessCorpusBin: Concrete class responsible for repairing control character presentations in Corpus objects.
#'   \item PreprocessCorpusEncode: Concrete class responsible for repairing non-UTF8 character presentations in Corpus objects.
#'   \item PreprocessCorpusReshape: Concrete class responsible for reshaping Corpus objects into tokens, sentences and/or paragraphs.
#'   \item PreprocessCorpusSplit: Concrete class responsible for splitting a Corpus object for cross-validation purposes.
#'   \item PreprocessCorpusParse: Concrete class responsible for parsing specified content types, such as URLs from text in Corpus objects.
#'   \item PreprocessCorpusNormalize: Concrete class responsible for normalizing text in Corpus objects.
#'   \item PreprocessCorpusFilter: Concrete class responsible for filtering specified text or substrings from Corpus objects.
#'  }
#'
#' \strong{Concrete Document Preprocess Classes:}
#'  \itemize{
#'   \item PreprocessDocumentBin: Concrete class responsible for repairing control character presentations in Document objects.
#'   \item PreprocessDocumentEncode: Concrete class responsible for repairing non-UTF8 character presentations in Document objects.
#'   \item PreprocessDocumentReshape: Concrete class responsible for reshaping Document objects into tokens, sentences and/or paragraphs.
#'   \item PreprocessDocumentSplit: Concrete class responsible for splitting a Document object for cross-validation purposes.
#'   \item PreprocessDocumentParse: Concrete class responsible for parsing specified content types, such as URLs from text in Document objects.
#'   \item PreprocessDocumentNormalize: Concrete class responsible for normalizing text in Document objects.
#'   \item PreprocessDocumentFilter: Concrete class responsible for filtering specified text or substrings from Document objects.
#'  }
