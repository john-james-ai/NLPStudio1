#==============================================================================#
#                                VIORead                                       #
#==============================================================================#
#' VIORead
#'
#'
#' \code{VIORead} Visitor class responsible for reading Document object content from file.
#'
#' @section VIORead Methods:
#'  \describe{
#'   \item{\code{documentTxt(document)}}{Method for reading a text file.}
#'   \item{\code{documentCsv(document)}}{Method for reading a csv file.}
#'   \item{\code{documentBin(document)}}{Method for reading a binary file.}
#'   \item{\code{documentRdata(document)}}{Method for reading an Rdata file.}
#'   \item{\code{documentRds(document)}}{Method for reading an Rds file.}
#'   \item{\code{documentXML(document)}}{Method for reading an XML file.}
#'   \item{\code{documentJSON(document)}}{Method for reading an JSON file.}
#' }
#'
#' @param document Document object
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family VIO family of classes
#' @export
VIORead <- R6::R6Class(
  classname = "VIORead",
  lock_objects = FALSE,
  lock_class = FALSE,

  public = list(

    initialize = function() {
      invisible(self)
    },

    documentTxt = function(document) {
      io <- IOText$new()
      return(io$read(document))
    },

    documentCsv = function(document) {
      io <- IOCSV$new()
      return(io$read(document))
    },

    documentBin = function(document) {
      io <- IOBin$new()
      return(io$read(document))
    },

    documentRds = function(document) {
      io <- IORds$new()
      return(io$read(document))
    },

    documentRdata = function(document) {
      io <- IORdata$new()
      return(io$read(document))
    },

    documentXML = function(document) {
      status = list()
      status[['code']]  <- FALSE
      status[['msg']] <- 'documentXML method not implemented.'
      return(status)
    },

    documentJSON = function(object) {
      status = list()
      status[['code']]  <- FALSE
      status[['msg']] <- 'documentJSON method not implemented.'
      return(status)
    }
  )
)
