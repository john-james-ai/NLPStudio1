## ---- IORdata
#==============================================================================#
#                                 IORdata                                      #
#==============================================================================#
#' IORdata
#'
#'
#' \code{IORdata} Class responsible for reading and writing Rdata files.
#'
#' \strong{IO Class Overview:}
#' The IORdata class is an implementation of the strategy design pattern,
#' as described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This strategy pattern accommodates
#' various formats and allows the behavior to be defined / selected at run time.
#'
#' \strong{IORdata Methods:}
#' The IORdata class supports csv, Rdata, and text files through the following methods:
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
IORdata <- R6::R6Class(
  classname = "IORdata",
  lock_objects = TRUE,
  lock_class = FALSE,
  inherit = IO0,
  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    read = function(fileObject) {

      path <- fileObject$getPath()
      fileName <- basename(path)

      if (file.exists(path)) {
        env <- new.env()
        content <- load(path, envir = env)
        content <- env[[content]]
        private$..state <- paste0("Successfully read ", fileName, ".")
        private$..accessed <- Sys.time()
        self$logIt()
      } else {
        private$..state <- paste0('Unable to read ', fileName, '. ',
                                  'File does not exist.')
        self$logIt("Error")
        stop()
      }
      return(content)
    },

    write = function(fileObject, content) {

      path <- fileObject$getPath()
      fileName <- basename(path)
      dirName <- dirname(path)

      # Create directory if necessary
      dir.create(dirName, showWarnings = FALSE, recursive = TRUE)

      save(object = content, file = path, compression_level = 9)

      private$..state <- paste0("Successfully wrote ", fileName, ".")
      self$logIt()

      private$..created <- Sys.time()
      private$..modified <- Sys.time()
      private$..accessed <- Sys.time()

      invisible(self)
    }
  )
)
