## ---- IOText
#==============================================================================#
#                                      IOText                                  #
#==============================================================================#
#' IOText
#'
#'
#' \code{IOText} Class responsible for reading and writing text documents.
#'
#' \strong{IO Class Overview:}
#' The IOText class is an implementation of the strategy design pattern,
#' as described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This strategy pattern accommodates
#' various formats and allows the behavior to be defined / selected at run time.
#'
#' \strong{IOText Methods:}
#' The IOText class supports csv, Rdata, and text files through the following methods:
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
IOText <- R6::R6Class(
  classname = "IOText",
  lock_objects = TRUE,
  lock_class = FALSE,
  private = list(),
  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(document) {

      status <- list()
      status[['code']] <- TRUE

      filePath <- document$getFilePath()
      fileName <- basename(filePath)

      if (file.exists(filePath)) {
        con <- file(filePath)
        on.exit(close(con))
        status[['data']] <- readLines(con)
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
        con <- file(filePath)
        on.exit(close(con))
        writeLines(content, con)
      } else {
        status[['code']] <- FALSE
        status[['msg']] <- paste0('Unable to write ', fileName, '. ',
                                  'Document content is NULL.')
      }
      return(status)
    }
  )
)
