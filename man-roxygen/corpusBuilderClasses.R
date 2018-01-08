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
#'  \item CorpusBuilderRawText: Concrete builder of raw Corpus objects sourced from text sources.
#'  \item CorpusBuilderRawCSV: Concrete builder of raw Corpus objects sourced from CSV sources.
#'  \item CorpusBuilderRawJSON: Concrete builder of raw Corpus objects sourced from JSON sources.
#'  \item CorpusBuilderRawXML: Concrete builder of raw Corpus objects sourced from XML sources.
#'  \item CorpusBuilderRawQuanteda: Concrete builder of raw Corpus objects sourced Quanteda corpus objects.
#'  \item CorpusBuilderRawTM: Concrete builder of raw Corpus objects sourced TM VCorpus  objects.
#'  \item CorpusBuilderRawKoRpus: Concrete builder of raw Corpus objects sourced KoRpus package objects.
#'  \item CorpusBuilderRepair: Concrete builder of repaired Corpus objects.
#'  \item CorpusBuilderReshape: Concrete builder of reshaped Corpus objects.
#'  \item CorpusBuilderSplitHoldOut: Concrete builder of Corpus objects split into training, validation and test sets.
#'  \item CorpusBuilderSplitKFold: Concrete builder of Corpus objects split according to the K-Fold cross-validation strategy.
#'  \item CorpusBuilderProcessed: Concrete builder of the processed Corpus objects.
#'  }
