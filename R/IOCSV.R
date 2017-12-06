## ---- IOCSV
#==============================================================================#
#                                      IOCSV                                   #
#==============================================================================#
#' IOCSV
#'
#'
#' \code{IOCSV} Class responsible for reading and writing csv documents.
#'
#' \strong{IO Class Overview:}
#' The IOCSV class is an implementation of the strategy design pattern,
#' as described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This strategy pattern accommodates
#' various formats and allows the behavior to be defined / selected at run time.
#'
#' \strong{IOCSV Methods:}
#' The IOCSV class supports csv, Rdata, and text files through the following methods:
#'  \itemize{
#'   \item{\code{read(document)}}{Read method.}
#'   \item{\code{write(document)}}{Write method.}
#' }
#'
#' @param document Object of the Document family of classes
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Input / Output Classes
#' @export
IOCSV <- R6::R6Class(
  classname = "IOCSV",
  lock_objects = TRUE,
  lock_class = FALSE,
  private = list(),
  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(document, header = TRUE) {

      status <- list()
      status[['code']] <- TRUE

      filePath <- document$getFilePath()
      fileName <- basename(filePath)

      if (file.exists(filePath)) {
        status[['data']] <- read.csv(file = filePath, header = header,
                                     stringsAsFactors = FALSE,
                                     sep = ",", quote = "\"'")
      } else {
        status[['code']] <- FALSE
        status[['msg']] <- paste0('Unable to read ', fileName, '. ',
                                  'File does not exist.')
      }
      return(status)
    },

    write = function(document) {

      status <- list()
      status[['code']] <- TRUE

      filePath <- document$getFilePath()
      fileName <- basename(filePath)
      content <- document$getContent()

      if (!is.null(content)) {
        write.csv(content, file = filePath, row.names = FALSE)
      } else {
        status[['code']] <- FALSE
        status[['msg']] <- paste0('Unable to write ', fileName, '. ',
                                  'Document content is NULL.')
      }
      return(status)
    }
  )
)
