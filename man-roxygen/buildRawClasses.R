#' @section BuildDataRaw Family of Classes Overview:
#' Abstract class for the build raw data classes. Attending sub-classes take
#' text data in a variety of formats and builds a Corpus object, stores the
#' Corpus text to disk and returns the Corpus object to the calling
#' environmnt.  The attending subclasses include:
#' \itemize{
#'  \item BuildDataRawText: Builds a raw data Corpus object from text sources.
#'  \item BuildDataRawCSV: Builds a raw data Corpus object from CSV sources.
#'  \item BuildDataRawXML: Builds a raw data Corpus object from XML sources.
#'  \item BuildDataRawJSON: Builds a raw data Corpus object from JSON sources.
#'  \item BuildDataRawQuanteda: Builds a raw data Corpus object from a Quanteda corpus object.
#'  \item BuildDataRawTM: Builds a raw data Corpus object from a TM VCorpus object.
#'  \item BuildDataRawKoRpus: Builds a raw data Corpus object from a KoRpus object.
#' }
