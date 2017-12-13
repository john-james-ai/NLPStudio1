#==============================================================================#
#                                VIOWrite                                      #
#==============================================================================#
#' VIOWrite
#'
#'
#' \code{VIOWrite} Visitor class responsible for writing File object to disc.
#'
#' @section VIOWrite Methods:
#'  \describe{
#'   \item{\code{fileTXT(file)}}{Method for writing a text file.}
#'   \item{\code{fileCSV(file)}}{Method for writing a csv file.}
#'   \item{\code{fileBin(file)}}{Method for writing a binary file.}
#'   \item{\code{fileRdata(file)}}{Method for writing an Rdata file.}
#'   \item{\code{fileRDS(file)}}{Method for writing an RDS file.}
#'   \item{\code{fileXML(file)}}{Method for writing an XML file.}
#'   \item{\code{fileJSON(file)}}{Method for writing an JSON file.}
#' }
#'
#' @param file Document object
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

    fileTXT = function(file) {
      io <- IOText$new()
      return(io$write(file))
    },

    fileCSV = function(file) {
      io <- IOCSV$new()
      return(io$write(file))
    },

    fileBin = function(file) {
      io <- IOBin$new()
      return(io$write(file))
    },

    fileRDS = function(file) {
      io <- IORDS$new()
      return(io$write(file))
    },

    fileRdata = function(file) {
      io <- IORdata$new()
      return(io$write(file))
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
