## ---- DataSourceWebComp
#==============================================================================#
#                           DataSourceWebComp                                  #
#==============================================================================#
#' DataSourceWebComp
#'
#'
#' \code{DataSourceWebComp} Class responsible for obtaining file collections from compressed web sources
#'
#' Class downloads the file collection from its web source in a compressed format
#' and saves it in the sibling directory "external". A FileCollection object with the
#' name and path designated by the parameters is created. The "external" file
#' is unzipped into the FileCollection's path, loaded into the FileCollection
#' object and it is returned to the calling environment.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new(name, path, params)}}{Instantiates an object of one of the DataSource classes.}
#'  \item{\code{execute()}}{Sources the data, creates the FileCollection object, and returns it to the calling environment.}
#'  \item{\code{accept(visitor)}}{Accepts visitor object.}
#' }
#'
#' @return collection FileCollection object
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Data source classes
#' @export
DataSourceWebComp <- R6::R6Class(
  "DataSourceWebComp",
  inherit = DataSource0,

  private = list(
    ..url = character(),
    ..zipFiles = character()
  ),

  #-------------------------------------------------------------------------#
  #                              Core Methods                               #
  #-------------------------------------------------------------------------#
  public = list(
    initialize = function(name, path, params) {

      private$..className <- 'DataSourceWebComp'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..path <- path
      private$..params <- params
      private$..url <- params[[1]]
      private$..zipFiles <- params[[2]]
      private$..state <- paste("DataSourceWebComp object instantiated.")
      private$..logs <- LogR$new()
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..accessed <- Sys.time()

      # Validate Source
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      invisible(self)
    },

    execute = function() {

      private$..methodName = 'execute'

      # Designate the external download directory
      downloadDir <- file.path(dirname(private$..path), 'external')

      # Create directories
      dir.create(private$..path, showWarnings = FALSE, recursive = TRUE)
      dir.create(downloadDir, showWarnings = FALSE, recursive = TRUE)

      # Download file
      fileName <- installr::file.name.from.url(url)
      filePath <- file.path(downloadDir, fileName)
      if (download.file(url, destfile = filePath, mode = 'wb') != 0) {
        private$..state <- paste0("Unable to download ", fileName, ".")
        self$logIt('Error')
        stop()
      }

      # Unzip Files
      files <- unzip(zipfile = filePath, overwrite = TRUE,
                     exdir = private$..path, junkpaths = TRUE,
                     files = private$..zipFiles)

      # Create file collection, file objects and add to file collection
      fc <- FileCollection$new(name = private$..name, path = private$..path)
      lapply(files, function(f) {
        fileName <- basename(f)
        path <- file.path(private$..path, fileName)
        file <- File$new(name = tools::file_path_sans_ext(fileName), path = path)
        file$read()
        fc$addFile(file)
      })

      # Lock the data (assumed to be raw data that is immutable)
      fc$lock()

      private$..state <-  paste0("Successfully sourced ", private$..name, ".")
      self$logIt()

      return(fc)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$dataSourceWebComp(self)
    }
  )
)

