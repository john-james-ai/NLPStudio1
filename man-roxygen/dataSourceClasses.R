#' @section DataSource Family of Classes Overview:
#' Abstract class for the data source classes. Attending sub-classes include:
#' \itemize{
#'  \item DataSourceText: Specifies a text format source or the relative path location thereto.
#'  \item DataSourceCSV: Specifies a CSV format source or the relative path location thereto.
#'  \item DataSourceXML: Specifies an XML format source or the relative path location thereto.
#'  \item DataSourceJSON: Specifies the URL to a JSON format source.
#'  \item DataSourceQuanteda: Specifies a Quanteda corpus object or the relative path location thereto.
#'  \item DataSourceTM: Specifies a TM VCorpus object or the relative path location thereto.
#'  \item DataSourceKoRpus: Specifies a KoRpus package corpus object or the relative path location thereto.
#' }
