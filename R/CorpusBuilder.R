#==============================================================================#
#                             CorpusBuilder                                    #
#==============================================================================#
#' CorpusBuilder
#'
#' \code{CorpusBuilder} Class obtains the data and creates the Corpus Object.
#'
#' Class contains a group of file object and all methods required for reading, writing,
#' downloading, compressing, and repairing files.
#'
#' @section CorpusBuilder Methods:
#'  \describe{
#'   \item{\code{new(name, path)}}{Instantiates a CorpusBuilder object and initiatesa Corpus object. }
#'   \item{\code{download(directory, url)}}Creates a FileCollection object, then downloads file collection from the url into the designated directory.}
#'   \item{\code{unZip(collection, directory, zipFiles,)}}{Unzips the files from the zipCollection object, then returns an raw FileCollection object.}
#'   \item{\code{repair(collection, directory)}}{Takes a FileCollection object, repairs the encoding, and returns a the repaired FileCollection object. }
#'   \item{\code{construct(collection, directory)}}{Takes a FileCollection object, and creates a Corpus object. }

#'  }
#'
#' @section Parameters:
#' @param collection FileCollection object.
#' @param directory Character string indicating a directory name.
#' @param name A character string containing the name of the CorpusBuilder object.
#' @param path Character string indicating the directory location for the File Collection object.
#' @param url Character string containing the URL from which a file collection will be downloaded.
#' @param zipFile Character string containing the relative path to the zipFile.
#' @param zipFiles Cheracter vector containing the relative paths of the files to be extracted.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
CorpusBuilder <- R6::R6Class(
  classname = "CorpusBuilder",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..corpus = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         CorpusBuilder Core Methods                      #
    #-------------------------------------------------------------------------#
    initialize = function(name, path) {

      private$..className <- 'CorpusBuilder'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..path <- path
      private$..io <- NULL
      private$..state <- paste("CorpusBuilder object", private$..name, "instantiated.")
      private$..logs <- LogR$new()
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..accessed <- Sys.time()

      dir.create(private$..path, showWarnings = FALSE, recursive = TRUE)

      invisible(self)
    },

    getPath = function() private$..path,

    #-------------------------------------------------------------------------#
    #                          Download Method                                #
    #-------------------------------------------------------------------------#
    download = function(url, directory) {

      private$..methodName <- 'download'

      # Format download directory
      fileName <- installr::file.name.from.url(url)

      # Format download file path
      filePath <- file.path(private$..path, directory, fileName)

      if (download.file(url, destfile = file.path(directory, fileName), mode = 'wb') != 0) {
        private$..state <- paste0("Unable to download ", fileName, ".")
        self$logIt('Error')
        stop()
      }
      private$..state <- paste0("Successfully downloaded ", fileName, ". ")
      private$..created <- Sys.time()
      private$..modified <- Sys.time()
      private$..accessed <- Sys.time()
      self$logIt()

      return(filePath)
    },

    #-------------------------------------------------------------------------#
    #                          Zip/Unzip Methods                              #
    #-------------------------------------------------------------------------#
    zipFile = function(zipFile, zipFiles) {

      rc <- zip(zipfile = zipFile, files = zipFiles)
      if (rc == 0) {
        private$..state <- paste0('Successfully zipped ', basename(zipFile), ".")
        private$..created <- Sys.time()
        private$..modified <- Sys.time()
        private$..accessed <- Sys.time()
        self$logIt()
      } else {
        private..state <- paste0("Unable to zip ", basename(zipFile), ".")
        self$logIt('Error')
        stop()
      }
      invisible(self)
    },

    unZipFile = function(zipFile, directory, zipFiles = NULL, listFiles = FALSE,
                         overwrite = TRUE) {

      if (file.exists(zipFile)) {
        unzip(zipfile = zipFile, overwrite = overwrite,
              exdir = file.zipFile(private..path, directory),
              junkzipFiles = TRUE, files = zipFiles, list = listFiles)
        private$..state <-  paste0("Successfully unzipped ", basename(zipFile), ".")
        private$..created <- Sys.time()
        private$..modified <- Sys.time()
        private$..accessed <- Sys.time()
        self$logIt()
      } else {
        private$..state <-  paste0("Could not unzip ", basename(zipFile),
                                   ". File does not exist.")
        self$logIt('Error')
        stop()
      }
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                            Repair Method(s)                             #
    #-------------------------------------------------------------------------#
    repair = function(collection, directory) {

      # Send collection to Processor Repair class
    }
  )
)
