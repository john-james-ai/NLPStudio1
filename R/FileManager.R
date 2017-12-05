#==============================================================================#
#                              FileManager                                     #
#==============================================================================#
#' FileManager
#'
#' \code{FileManager} Class containing file management functions
#'
#' This class contains functions for maintaining and manipulating files
#'
#' @section: FileManager Methods:
#'  \describe{
#'   \item{\code{new()}}{Creates an object of FileManager Class}
#'   \item{\code{move(from, to)}}{Move a file or directory from directory a to directory b.}
#'   \item{\code{copy(from, to)}}{Copy a file or directory from directory a to directory b.}
#'   \item{\code{zip(zipFile, files)}}{Compresses files or files and stores them in the filepath indicated by zipFile.}
#'   \item{\code{unzip(zipFile, exDir)}}{Extract compressed files and places them in the directory indicated by exDir.}
#'   \item{\code{download(url, destFile)}}{Downloads a file and stores it in .}
#'   \item{\code{checkDir(directory)}}{Creates a directory, if it already exists, it returns the size of the directory.}
#'  }
#'
#' @section Parameters:
#' @param name A character string containing the name of the FileManager object. This variable is used in the instantiation and remove methods.
#' @param desc A chararacter string containing the description of the FileManager
#' @param document An object of the DocumentCollection class to be added to the FileManager object's list of document collections.
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
FileManager <- R6::R6Class(
  classname = "FileManager",
  lock_objects = FALSE,
  lock_class = FALSE,
  private = list(
    ..name = "fileManager",
    ..state = character(),
    ..created = character(),
    ..modified = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         FileManager Core Methods                        #
    #-------------------------------------------------------------------------#
    initialize = function() {

      # Instantiate variables
      private$..state <- paste("FileManager", name, "instantiated at", Sys.time())
      private$..modified <- Sys.time()
      private$..created <- Sys.time()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                          Download Method                                #
    #-------------------------------------------------------------------------#
    download = function(url, downloadPath) {
      status <- list()
      status[['code']] <- TRUE
      if (download.file(url, destfile = downloadPath, mode = 'wb') != 0) {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Could not download ", url)
      }
      return(status)
    },

    #-------------------------------------------------------------------------#
    #                          Zip/Unzip Methods                              #
    #-------------------------------------------------------------------------#
    zipFile = function(zipFilePath, files) {
      status <- list()
      status[['code']] <- TRUE
      zip(zipfile = zipFilePath, files = files)
      return(status)
    },

    unZipFile = function(zipFilePath, files, exDir) {
      status <- list()
      status[['code']] <- TRUE
      if (file.exists(zipFilePath)) {
        unzip(zipfile = zipFilePath, overwrite = FALSE, exdir = exDir,
              junkpaths = TRUE, files = files)
      } else {
        status[['code']] <- FALSE
        status[['msg']] <- paste0("Could not unzip ", zipFilePath,
                                  ". File does not exist.")
      }
      return(status)
    },


    #-------------------------------------------------------------------------#
    #                         Move/Copy Methods                               #
    #-------------------------------------------------------------------------#
    moveFile = function(from, to)  {
      todir <- dirname(to)
      if (!isTRUE(file.info(todir)$isdir))  dir.create(todir, recursive=TRUE)
      file.rename(from = from,  to = to)
    },

    copyFile = function(from, to)  {
      todir <- dirname(to)
      if (!isTRUE(file.info(todir)$isdir))  dir.create(todir, recursive=TRUE)
      file.copy(from = from,  to = to)
    }
  )
)
