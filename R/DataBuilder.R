#==============================================================================#
#                             DataBuilder                                      #
#==============================================================================#
#' DataBuilder
#'
#' \code{DataBuilder} Class responsible for building the data sets for a pipeline.
#'
#' Class obtains the source data from external sources, creates the raw data,
#' creates a refined data set with eoncoding errors corrected, splits the data
#' into cross validation sets and creates the corpus that will be used to
#' train the model(s)
#'
#' @section DataBuilder Methods:
#'  \describe{
#'   \item{\code{new(name, path)}}{Instantiates a DataBuilder object and initiatesa Data object. }
#'   \item{\code{download(directory, url)}}{Creates a FileCollection object, then downloads file collection from the url into the designated directory.}
#'   \item{\code{unZip(collection, directory, zipFiles,)}}{Unzips the files from the zipCollection object, then returns an raw FileCollection object.}
#'   \item{\code{repair(collection, directory)}}{Takes a FileCollection object, repairs the encoding, and returns a the repaired FileCollection object. }
#'   \item{\code{split(collection, directory)}}{Splits the data sets into training, validation and test sets. }
#'   \item{\code{construct(collection, directory)}}{Returns the cross validation sets. }
#'  }
#'
#' @section Parameters:
#' @param collection FileCollection object.
#' @param directory Character string indicating a directory name.
#' @param name A character string containing the name of the DataBuilder object.
#' @param path Character string indicating the directory location for the File Collection object.
#' @param url Character string containing the URL from which a file collection will be downloaded.
#' @param zipFile Character string containing the relative path to the zipFile.
#' @param zipFiles Cheracter vector containing the relative paths of the files to be extracted.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
DataBuilder <- R6::R6Class(
  classname = "DataBuilder",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..data = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         DataBuilder Core Methods                        #
    #-------------------------------------------------------------------------#
    initialize = function(name, path) {

      private$..className <- 'DataBuilder'
      private$..methodName <- 'initialize'
      private$..state <- paste("DataBuilder object", private$..name, "instantiated.")
      private$..logs <- LogR$new()
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..accessed <- Sys.time()

      # Create Data object
      private$..data <- Data$new(name = name, path = path)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                          Download Method                                #
    #-------------------------------------------------------------------------#
    download = function(url, name) {

      private$..methodName = 'download'

      # Get Data object path
      path <- private$..data$getPath()

      # Create new file collection
      fc <- FileCollection$new(name = name, path = file.path(path, name))

      # Download
      fc <- fc$download(url)

      # Log
      private$..state <- paste0("Downloaded ", name, " file collection.")
      self$logIt()

      private$..data <- private$..data$addCollection(fc)

      return(fc)
    },

    #-------------------------------------------------------------------------#
    #                          Zip/Unzip Methods                              #
    #-------------------------------------------------------------------------#
    zipFile = function(fc, name, zipFilePath) {

      private$..methodName <- 'zipFile'

      # Get Data object path
      path <- private$..data$getPath()

      # Create fileCollection Object
      newFc <- FileCollection$new(name = name,
                                  path = file.path(path, name))

      # Zip Data
      zipFiles <- list.files(fc$getPath(), full.names = TRUE)
      rc <- zip(zipfile = zipFilePath, files = zipFiles)
      if (rc == 0) {
        private$..state <- paste0('Successfully zipped ', basename(zipFile), ".")
        self$logIt()
      } else {
        private..state <- paste0("Unable to zip ", basename(zipFile), ".")
        self$logIt('Error')
        stop()
      }

      # Add new file collection to Data object
      private$..data <- private$..data$addCollection(newFc)

      return(newFc)
    },

    unZipFile = function(fc, name, zipFiles = NULL, listFiles = FALSE,
                         overwrite = TRUE) {

      private$..methodName <- 'unZipFile'

      # Get Data object path
      path <- private$..data$getPath()

      # Create fileCollection Object
      newFc <- FileCollection$new(name = name, path = file.path(path, name))

      # Unzip Files
      zipFile <- fc$getFilePaths()[[1]]
      exDir <- file.path(path, name)
      if (file.exists(zipFile)) {
        files <- unzip(zipfile = zipFile, overwrite = overwrite,
                       exdir = exDir, junkpaths = TRUE, files = zipFiles,
                       list = listFiles)

        lapply(files, function(f) {
          newFc$addFilePath(f)
        })

        private$..state <-  paste0("Successfully unzipped ", basename(zipFile), ".")
        self$logIt()
      } else {
        private$..state <-  paste0("Could not unzip ", basename(zipFile),
                                   ". File does not exist.")
        self$logIt('Error')
        stop()
      }

      # Add new file collection to Data object
      private$..data <- private$..data$addCollection(newFc)

      return(newFc)
    },

    #-------------------------------------------------------------------------#
    #                            Repair Method(s)                             #
    #-------------------------------------------------------------------------#
    repair = function(fc, name) {

      private$..methodName <- 'repair'

      # Get Data object path
      path <- private$..data$getPath()

      # Create new file collection
      newFc <- FileCollection$new(name = name,
                                  path = file.path(path, name))

      files <- fc$getFilePaths()
      lapply(files, function(f) {

        # Read and repair data
        io <- IOBin$new()
        d <- io$read(f)
        d[d == as.raw(0)] = as.raw(0x20)
        d[d == as.raw(26)] = as.raw(0x20)
        temp <- tempfile(fileext = '.txt')

        writeBin(d, temp)
        d <- readLines(temp)
        unlink(temp)

        # Write data to new file collection
        newFc$write(f, d, io)
      })

      private$..state <-  paste0("Successfully repaired ", outFc$getName(), ".")
      private$..created <- Sys.time()
      private$..modified <- Sys.time()
      private$..accessed <- Sys.time()

      # Add new file collection to Data object
      private$..data <- private$..data$addCollection(newFc)

      return(newFc)

    },

    #-------------------------------------------------------------------------#
    #                            Explose Object                               #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      o <- list(
        className	 =  private$..className ,
        name	 = 	    private$..name ,
        path	 = 	    private$..path ,
        files = private$..files,
        corpora = private$..corpora,
        state	 = 	    private$..state ,
        logs	 = 	    private$..logs ,
        modified	 = 	private$..modified ,
        created	 = 	  private$..created ,
        accessed	 = 	private$..accessed
      )
      return(o)
    }

  )
)
