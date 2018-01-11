#==============================================================================#
#                               DataSourceText                                 #
#==============================================================================#
#' DataSourceText
#'
#' \code{DataSourceText} Concrete class for defining text data sources.
#'
#' @template dataSourceClasses
#' @template  dataSourceMethods
#' @template  dataSourceParams
#'
#' @return dataSource: An object of one of the DataSourceText class.
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Source data classes
#' @export
DataSourceText <- R6::R6Class(
  "DataSourceText",
  inherit = DataSource0,

  public = list(
    initialize = function(dataSource) {

      private$..dataSource <- dataSource
      private$..className <- 'DataSourceText'
      private$..methodName <- 'initialize'
      private$..state <- paste0("DataSourceText object instantiated.")
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..accessed <- Sys.time()
      private$..logs <- LogR$new()

      invisible(self)
    },

    getSource = function() {

      private$..methodName <- 'getSource'

      io <- IOText$new()

      # If directory, create list of character vectors, one per file.
      if (class(private$..dataSource) == "list") {
        lapply(seq_along(private$..dataSource), function(x) {
          if (class(private$..dataSource[[x]]) != "character") {
            private$..state <- paste0("Unable to read the data source. ",
                                      "List includes none character vector elements. ",
                                      "See ?", class(self)[1], "for further ",
                                      "assistance. ")
            self$logIt("Error")
            stop()
          }
        })
        private$..documents <- private$..dataSource
        if (is.null(names(private$..documents))) {
          n <- unlist(lapply(seq_along(private$..documents), function(x) {
            paste0("Document", x, collapse = "")
          }))
          names(private$..documents) <- n
        }

      } else  if (isDirectory(private$..dataSource[[1]])) {
        files <- list.files(private$..dataSource, full.names = TRUE)
        private$..documents <- lapply(files, function(f) {
          io$read(f)
        })
        docNames <- lapply(files, function(f) {
          tools::file_path_sans_ext(basename(f))
        })
        names(private$..documents) <- docNames

      } else if (isFile(private$..dataSource[[1]])) {
        private$..documents <- io$read(private$..dataSource)

      } else if (class(private$..dataSource) == "character") {
        private$..documents <- private$..dataSource
      }

      return(private$..documents)
    }
  )
)
