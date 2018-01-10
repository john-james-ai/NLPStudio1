#==============================================================================#
#                               DataSourceCSV                                  #
#==============================================================================#
#' DataSourceCSV
#'
#' \code{DataSourceCSV} Concrete class for defining CSV data sources.
#'
#' @template dataSourceClasses
#' @template  dataSourceMethods
#' @template  dataSourceParams
#'
#' @return dataSource: An object of one of the DataSourceCSV class.
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Source data classes
#' @export
DataSourceCSV <- R6::R6Class(
  "DataSourceCSV",
  inherit = DataSource0,

  public = list(
    initialize = function(dataSource) {

      private$..dataSource <- dataSource
      private$..className <- 'DataSourceCSV'
      private$..methodName <- 'initialize'
      private$..state <- paste0("DataSourceCSV object instantiated.")
      private$..meta[["modified"]] <- Sys.time()
      private$..meta[["created"]] <- Sys.time()
      private$..meta[["accessed"]] <- Sys.time()
      private$..logs <- LogR$new()

      invisible(self)
    }
  )
)

