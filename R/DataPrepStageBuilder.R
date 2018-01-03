#==============================================================================#
#                             DataPrepStageBuilder                             #
#==============================================================================#
#' DataPrepStageBuilder
#'
#' \code{DataPrepStageBuilder} Class responsible for the data preparation stage of the pipeline.
#'
#' Class obtains the source data from external sources, creates the raw data,
#' creates a refined data set with encoding errors corrected, splits the data
#' into cross validation sets and creates the corpus that will be used to
#' train the model(s)
#'
#' @section DataPrepStageBuilder Methods:
#'  \describe{
#'   \item{\code{new(name, path)}}{Instantiates a DataPrepStageBuilder object and the Data object. }
#'   \item{\code{download(url name)}}{Downloads the data from the URL, stores the file collection and adds it to the Data object.}
#'   \item{\code{zipFile(fc, name, zipFilePath)}}{Zips the files in the given FileCollection object, creates a new FileCollection object and stores it the designated zipFilePath.}
#'   \item{\code{unZipFiles(fc, name, zipFiles)}}{Unzips the file in the designated FileCollection object, creates a new FileCollection object by the given name, extracts the zipFiles, stores them in the new FileCollection, and adds the FileCollection object to the Data object.}
#'   \item{\code{repair(fc, name)}}{Repairs the FileCollection object and stores it in the new FileCollection object which is added to the Data object.}
#'   \item{\code{construct()}}{Returns the cross validation sets. }
#'  }
#'
#' @section Parameters:
#' @param fc FileCollection object.
#' @param name A character string containing the name of the DataPrepStageBuilder object.
#' @param path Character string indicating the directory location for the File Collection object.
#' @param url Character string containing the URL from which a file collection will be downloaded.
#' @param zipFilePath Character string containing the relative path to the zipFile.
#' @param zipFiles Cheracter vector containing the relative paths of the files to be extracted.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
DataPrepStageBuilder <- R6::R6Class(
  classname = "DataPrepStageBuilder",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..dataPrep = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                     DataPrepStageBuilder Core Methods                   #
    #-------------------------------------------------------------------------#
    initialize = function(design) {

      private$..admin$className <- 'DataPrepStageBuilder'
      private$..admin$methodName <- 'initialize'
      private$..admin$state <- paste("DataPrepStageBuilder object instantiated.")
      private$..admin$logs <- LogR$new()
      private$..name <- 'dataBuilder'
      private$..data <- DataPrep$new(name = name, path = path)

      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                          Data Source Methods                            #
    #-------------------------------------------------------------------------#
    sourceData = function(dataSource) {
      fc <- dataSource$execute()
      private$..data$addData(fc)
      private$..admin$state <- paste0("Data obtained from source")
      self$logIt()
      return(fc)
    },


    #-------------------------------------------------------------------------#
    #                            Repair Method(s)                             #
    #-------------------------------------------------------------------------#
    repair = function(inFc, outFc) {

      private$..admin$methodName <- 'repair'

      # Confirm outFc directory is empty
      path <- outFc$getPath()
      if (dir.exists(path)) {
        private$..admin$state <- paste0("Unable to create new FileCollection at ",
                                  path, ". Directory is not empty.")
        self$logIt('Error')
        stop()
      }

      # Repair files iteratively
      files <- inFc$getFiles()
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
        outFc$addFile(newFile)
      })

      private$..admin$state <-  paste0("Successfully repaired ", name, ".")
      self$logIt()

      # Add corpus to Data object
      private$..data$addData(outFc)

      return(private$..data)

    },

    #-------------------------------------------------------------------------#
    #                            Reshape Method(s)                            #
    #-------------------------------------------------------------------------#
    reshape = function(inFc, outFc) {

      private$..admin$methodName <- 'reshape'

      # Confirm outFc directory is empty
      path <- outFc$getPath()
      if (dir.exists(path)) {
        private$..admin$state <- paste0("Unable to create new FileCollection at ",
                                        path, ". Directory is not empty.")
        self$logIt('Error')
        stop()
      }

      # Reshape files iteratively
      files <- inFc$getFiles()
      lapply(files, function(f) {

        # Read data
        d <- f$read()

        # Reshape Data
        sents <- quanteda::tokens(unlist(d), what = 'sentence')

        # Create new file
        fileName <- f$getFileName()
        filePath <- file.path(path, fileName)
        newFile <- File$new(name = f$getName(), path = filePath)

        # Write new file
        newFile$content <- sents
        newFile$write()
        outFc$addFile(newFile)
      })

      private$..admin$state <-  paste0("Successfully reshaped ", outFc$getName(), ".")
      private$..admin$created <- Sys.time()
      private$..admin$modified <- Sys.time()
      private$..admin$accessed <- Sys.time()

      # Add new file collection to Data object
      private$..data <- private$..data$addCollection(newFc)

      return(newFc)

    },


    #-------------------------------------------------------------------------#
    #                            Explose Object                               #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      o <- list(
        className	 =  private$..admin$className ,
        methodName = private$..admin$methodName,
        data = private$..data,
        state	 = 	    private$..admin$state ,
        logs	 = 	    private$..admin$logs ,
        modified	 = 	private$..admin$modified ,
        created	 = 	  private$..admin$created ,
        accessed	 = 	private$..admin$accessed
      )
      return(o)
    }

  )
)
