## ---- IOCSV
#==============================================================================#
#                                      IOCSV                                   #
#==============================================================================#
#' IOCSV
#'
#'
#' \code{IOCSV} Class responsible for reading and writing csv files.
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
#'   \item{\code{read(file)}}{Read method.}
#'   \item{\code{write(file)}}{Write method.}
#' }
#'
#' @param file Object of the File family of classes
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Input / Output Classes
#' @export
IOCSV <- R6::R6Class(
  classname = "IOCSV",
  lock_objects = TRUE,
  lock_class = FALSE,
  inherit = IO0,
  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(path, header = TRUE) {

      private$..admin$logs <- LogR$new()

      fileName <- basename(path)

      if (file.exists(path)) {
        content <- read.csv(file = path, header = header,
                            stringsAsFactors = FALSE,
                            sep = ",", quote = "\"'")
        private$..admin$state <- paste0("Successfully read ", fileName, ".")
        private$..admin$accessed <- Sys.time()
        self$logIt()
      } else {
        private$..admin$state <- paste0('Unable to read ', fileName, '. ',
                                  'File does not exist.')
        self$logIt("Error")
        stop()
      }
      return(content)
    },

    write = function(path, content) {

      private$..admin$logs <- LogR$new()

      fileName <- basename(path)
      dirName <- dirname(path)

      # Create directory if necessary
      dir.create(dirName, showWarnings = FALSE, recursive = TRUE)

      write.csv(content, file = path, row.names = FALSE)

      private$..admin$state <- paste0("Successfully wrote ", fileName, ".")
      self$logIt()

      private$..admin$created <- Sys.time()
      private$..admin$modified <- Sys.time()
      private$..admin$accessed <- Sys.time()

      invisible(self)
    }
  )
)
