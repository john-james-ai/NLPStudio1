## ---- IOText
#==============================================================================#
#                                      IOText                                  #
#==============================================================================#
#' IOText
#'
#'
#' \code{IOText} Class responsible for reading and writing text files.
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
#'   \item{\code{read(file)}}{Read method.}
#'   \item{\code{write(file)}}{Write method.}
#' }
#'
#' @param file Object of the File family of classes
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Input / Output Classes
#' @export
IOText <- R6::R6Class(
  classname = "IOText",
  lock_objects = TRUE,
  lock_class = FALSE,
  inherit = IO0,
  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(path) {

      status <- list()
      status[['code']] <- TRUE

      fileName <- basename(path)

      if (file.exists(path)) {
        con <- file(path)
        on.exit(close(con))
        status[['data']] <- readLines(con)
      } else {
        status[['code']] <- FALSE
        status[['msg']] <- paste0('Unable to read ', fileName, '. ',
                                  'File does not exist.')
      }
      return(status)
    },

    write = function(content, path) {

      status <- list()
      status[['code']] <- TRUE

      # Format directory names
      dirName <- dirname(path)
      fileName <- basename(path)

      # Create directory if necessary
      dir.create(dirName, showWarnings = FALSE, recursive = TRUE)

      con <- file(path)
      on.exit(close(con))
      writeLines(content, con)

      return(status)
    }
  )
)
