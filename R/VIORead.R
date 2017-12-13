#==============================================================================#
#                                VIORead                                       #
#==============================================================================#
#' VIORead
#'
#'
#' \code{VIORead} Visitor class responsible for reading File object content from file.
#'
#' @section VIORead Methods:
#'  \describe{
#'   \item{\code{fileTXT(file)}}{Method for reading a text file.}
#'   \item{\code{fileCSV(file)}}{Method for reading a csv file.}
#'   \item{\code{fileBin(file)}}{Method for reading a binary file.}
#'   \item{\code{fileRdata(file)}}{Method for reading an Rdata file.}
#'   \item{\code{fileRDS(file)}}{Method for reading an RDS file.}
#'   \item{\code{fileXML(file)}}{Method for reading an XML file.}
#'   \item{\code{fileJSON(file)}}{Method for reading an JSON file.}
#' }
#'
#' @param file Document object
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

    fileTXT = function(file) {
      io <- IOText$new()
      return(io$read(file))
    },

    fileCSV = function(file) {
      io <- IOCSV$new()
      return(io$read(file))
    },

    fileBin = function(file) {
      io <- IOBin$new()
      return(io$read(file))
    },

    fileRDS = function(file) {
      io <- IORDS$new()
      return(io$read(file))
    },

    fileRdata = function(file) {
      io <- IORdata$new()
      return(io$read(file))
    },

    fileXML = function(file) {
      status = list()
      status[['code']]  <- FALSE
      status[['msg']] <- 'fileXML method not implemented.'
      return(status)
    },

    fileJSON = function(object) {
      status = list()
      status[['code']]  <- FALSE
      status[['msg']] <- 'fileJSON method not implemented.'
      return(status)
    }
  )
)
