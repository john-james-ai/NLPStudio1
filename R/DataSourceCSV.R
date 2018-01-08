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
    initialize = function(name, dataSource) {

      private$..name <- name
      private$..dataSource <- dataSource
      private$..admin$className <- 'DataSourceCSV'
      private$..admin$methodName <- 'initialize'
      private$..admin$state <- paste0("DataSourceCSV object instantiated.")
      private$..admin$modified <- Sys.time()
      private$..admin$created <- Sys.time()
      private$..admin$accessed <- Sys.time()
      private$..admin$logs <- LogR$new()

      invisible(self)
    }
  )
)

