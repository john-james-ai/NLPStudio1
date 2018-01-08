#' @section CorpusBuilder Family of Classes Introduction:
#' The CorpusBuilder family of classes is an implementation of the builder design pattern,
#' as described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This builder pattern accommodates
#' the creation of different pipeline representations via separate
#' builder classes.
#'
#' \strong{CorpusBuilder Family of Classes Participants:}
#' \itemize{
#'  \item Corpus: Class that contains the collection of Document objects.
#'  \item CorpusDirector: Class responsible for building the Corpus product via the concrete Builder classes.
#'  \item CorpusBuilderRaw: Concrete builder of raw Corpus objects.
#'  \item CorpusBuilderRepair: Concrete builder of repaired Corpus objects.
#'  \item CorpusBuilderReshape: Concrete builder of reshaped Corpus objects.
#'  \item CorpusBuilderSplitHoldOut: Concrete builder of Corpus objects split into training, validation and test sets.
#'  \item CorpusBuilderSplitKFold: Concrete builder of Corpus objects split according to the K-Fold cross-validation strategy.
#'  \item CorpusBuilderProcessed: Concrete builder of the processed Corpus objects.
#'  }
