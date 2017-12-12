## ---- IOArchive
#==============================================================================#
#                                 IOArchive                                    #
#==============================================================================#
#' IOArchive
#'
#'
#' \code{IOArchive} Class responsible for reading and writing Archive documents.
#'
#' \strong{IO Class Overview:}
#' The IOArchive class is an implementation of the strategy design pattern,
#' as described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This strategy pattern accommodates
#' various formats and allows the behavior to be defined / selected at run time.
#'
#' \strong{IOArchive Methods:}
#' The IOArchive class supports csv, Archive, and text files through the following methods:
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
IOArchive <- R6::R6Class(
  classname = "IOArchive",
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

      filePath <- document$getPath()
      fileName <- basename(filePath)

      if (file.exists(filePath)) {
        env <- new.env()
        content <- load(filePath, envir = env)
        status[['data']] <- env[[content]]
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

      # Format directory names
      filePath <- document$getPath()
      dirName <- dirname(filePath)
      fileName <- basename(filePath)

      # Obtain content
      content <- document$getContent()

      # Create directory if necessary
      dir.create(dirName, showWarnings = FALSE, recursive = TRUE)

      if (!is.null(content)) {
        save(object = content, file = filePath, compression_level = 9)
      } else {
        status[['code']] <- FALSE
        status[['msg']] <- paste0('Unable to write ', fileName, '. ',
                                  'Document content is NULL.')
      }
      return(status)
    }
  )
)
