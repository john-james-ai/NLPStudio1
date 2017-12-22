## ---- DataSourceWeb
#==============================================================================#
#                               DataSourceWeb                                  #
#==============================================================================#
#' DataSourceWeb
#'
#'
#' \code{DataSourceWeb} Class responsible for obtaining file collections from web sources
#'
#' Class downloads file collections from web sources, creates a FileCollection object,
#' saves it in the designated directory and returns the FileCollection object to
#' calling environment.
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
DataSourceWeb <- R6::R6Class(
  "DataSourceWeb",
  inherit = DataSource0,

  private = list(
    ..url = character()
  ),

  #-------------------------------------------------------------------------#
  #                              Core Methods                               #
  #-------------------------------------------------------------------------#
  public = list(
    initialize = function(name, path, params) {

      private$..name <- name
      private$..path <- path
      private$..params <- params
      private$..url <- params[[1]]

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

      # Format file path
      fileName <- installr::file.name.from.url(url)
      filePath <- file.path(private$..path, fileName)

      if (download.file(url, destfile = filePath, mode = 'wb') != 0) {
        private$..state <- paste0("Unable to download ", fileName, ".")
        self$logIt('Error')
        stop()
      }

      # Create new file collection
      fc <- FileCollection$new(name = private$..name, path = private$..path)

      # Log
      private$..state <- paste0("Successfully downloaded ", fileName, ". ")
      self$logIt()

      return(fc)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$dataSourceWeb(self)
    }
  )
)

