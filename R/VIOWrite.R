#==============================================================================#
#                                VIOWrite                                      #
#==============================================================================#
#' VIOWrite
#'
#'
#' \code{VIOWrite} Visitor class responsible for writing Document object content to file.
#'
#' @section VIOWrite Methods:
#'  \describe{
#'   \item{\code{documentTxt(document)}}{Method for writing a text file.}
#'   \item{\code{documentCsv(document)}}{Method for writing a csv file.}
#'   \item{\code{documentBin(document)}}{Method for writing a binary file.}
#'   \item{\code{documentRdata(document)}}{Method for writing an Rdata file.}
#'   \item{\code{documentRds(document)}}{Method for writing an Rds file.}
#'   \item{\code{documentXML(document)}}{Method for writing an XML file.}
#'   \item{\code{documentJSON(document)}}{Method for writing an JSON file.}
#' }
#'
#' @param document Document object
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family VIO family of classes
#' @export
VIOWrite <- R6::R6Class(
  classname = "VIOWrite",
  lock_objects = FALSE,
  lock_class = FALSE,

  public = list(

    initialize = function() {
      invisible(self)
    },

    documentTxt = function(document) {
      io <- IOText$new()
      return(io$write(document))
    },

    documentCsv = function(document) {
      io <- IOCSV$new()
      return(io$write(document))
    },

    documentBin = function(document) {
      io <- IOBin$new()
      return(io$write(document))
    },

    documentRds = function(document) {
      io <- IORds$new()
      return(io$write(document))
    },

    documentRdata = function(document) {
      io <- IORdata$new()
      return(io$write(document))
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
