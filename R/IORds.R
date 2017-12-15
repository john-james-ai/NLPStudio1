## ---- IORDS
#==============================================================================#
#                                      IORDS                                   #
#==============================================================================#
#' IORDS
#'
#'
#' \code{IORDS} Class responsible for reading and writing rdata files.
#'
#' \strong{IO Class Overview:}
#' The IORDS class is an implementation of the strategy design pattern,
#' as described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This strategy pattern accommodates
#' various formats and allows the behavior to be defined / selected at run time.
#'
#' \strong{IORDS Methods:}
#' The IORDS class supports csv, RDS, and text files through the following methods:
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
IORDS <- R6::R6Class(
  classname = "IORDS",
  lock_objects = TRUE,
  lock_class = FALSE,
  inherit = IO0,
  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(path, header = TRUE) {

      status <- list()
      status[['code']] <- TRUE

      fileName <- basename(path)

      if (file.exists(path)) {
        status[['data']] <- readRDS(file = path)
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

      saveRDS(object = content, file = path)

      return(status)
    }
  )
)
