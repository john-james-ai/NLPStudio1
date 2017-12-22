#==============================================================================#
#                             DataBuilder                                      #
#==============================================================================#
#' DataBuilder
#'
#' \code{DataBuilder} Class responsible for building the data sets for the pipeline.
#'
#' Class obtains the source data from external sources, creates the raw data,
#' creates a refined data set with encoding errors corrected, splits the data
#' into cross validation sets and creates the corpus that will be used to
#' train the model(s)
#'
#' @section DataBuilder Methods:
#'  \describe{
#'   \item{\code{new(name, path)}}{Instantiates a DataBuilder object and the Data object. }
#'   \item{\code{download(url name)}}{Downloads the data from the URL, stores the file collection and adds it to the Data object.}
#'   \item{\code{zipFile(fc, name, zipFilePath)}}{Zips the files in the given FileCollection object, creates a new FileCollection object and stores it the designated zipFilePath.}
#'   \item{\code{unZipFiles(fc, name, zipFiles)}}{Unzips the file in the designated FileCollection object, creates a new FileCollection object by the given name, extracts the zipFiles, stores them in the new FileCollection, and adds the FileCollection object to the Data object.}
#'   \item{\code{repair(fc, name)}}{Repairs the FileCollection object and stores it in the new FileCollection object which is added to the Data object.}
#'   \item{\code{construct()}}{Returns the cross validation sets. }
#'  }
#'
#' @section Parameters:
#' @param fc FileCollection object.
#' @param name A character string containing the name of the DataBuilder object.
#' @param path Character string indicating the directory location for the File Collection object.
#' @param url Character string containing the URL from which a file collection will be downloaded.
#' @param zipFilePath Character string containing the relative path to the zipFile.
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
    ..data = list(),

    parsePath = function(path) {

      if (isDirectory(path)) {
        files <- list.files(path = path, full.names = TRUE)
      } else {
        wildcard <- basename(path)
        dirName <- dirname(path)
        files <- list.files(path = dirName, pattern = wildcard, full.names = TRUE)
      }

      if (length(files) == 0) {
        private$..state <- paste0("Unable to instantiate corpus. Files not found.")
        self$logIt('Error')
        stop()
      }
      return(files)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         DataBuilder Core Methods                        #
    #-------------------------------------------------------------------------#
    initialize = function() {

      private$..className <- 'DataBuilder'
      private$..methodName <- 'initialize'
      private$..state <- paste("DataBuilder object", private$..name, "instantiated.")
      private$..logs <- LogR$new()

      invisible(self)
    },

    getData = function() private$..data,


    #-------------------------------------------------------------------------#
    #                            Repair Method(s)                             #
    #-------------------------------------------------------------------------#
    repair = function(fc, name, path) {

      private$..methodName <- 'repair'

      # Confirm directory is empty
      if (dir.exists(path)) {
        private$..state <- paste0("Unable to create new FileCollection at ",
                                  path, ". Directory is not empty.")
        self$logIt('Error')
        stop()
      }

      # Create repaired file collection
      newFc <- FileCollection$new(name = name, path = path)

      # Repair files iteratively
      files <- fc$getFiles()
      lapply(files, function(f) {

        # Read data
        io <- IOBin$new()
        d <- f$read(io)

        # Repair Data
        d[d == as.raw(0)] = as.raw(0x20)
        d[d == as.raw(26)] = as.raw(0x20)
        temp <- tempfile(fileext = '.txt')
        writeBin(d, temp)
        d <- readLines(temp)
        unlink(temp)

        # Create new file
        fileName <- f$getFileName()
        filePath <- file.path(path, fileName)
        newFile <- File$new(name = f$getName(), path = filePath)

        # Write new file
        newFile$content <- d
        newFile$write()
        newFc$addFile(newFile)
      })

      private$..state <-  paste0("Successfully repaired ", name, ".")
      self$logIt()

      # Add corpus to Data object
      private$..data[[name]] <- newFc

      return(newFc)

    },

    #-------------------------------------------------------------------------#
    #                            Reshape Method(s)                            #
    #-------------------------------------------------------------------------#
    reshape = function(korpus) {

      private$..methodName <- 'reshape'

      # Reshape the corpus into sentences
      sentCorpus <- tokens(korpus, what = 'sentence')
      sentCorpus <- tolower(sentCorpus)

      private$..state <-  paste0("Successfully reshaped ", outFc$getName(), ".")
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
        methodName = private$..methodName,
        data = private$..data,
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
