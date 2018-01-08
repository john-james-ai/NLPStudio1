#==============================================================================#
#                               DataSourceQuanteda                             #
#==============================================================================#
#' DataSourceQuanteda
#'
#' \code{DataSourceQuanteda} Concrete class for defining Quanteda data sources.
#'
#' @template dataSourceClasses
#' @template  dataSourceMethods
#' @template  dataSourceParams
#'
#' @return dataSource: An object of one of the DataSourceQuanteda class.
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Source data classes
#' @export
DataSourceQuanteda <- R6::R6Class(
  "DataSourceQuanteda",
  inherit = DataSource0,

  public = list(
    initialize = function(name, dataSource) {

      private$..name <- name
      private$..dataSource <- dataSource
      private$..admin$className <- 'DataSourceQuanteda'
      private$..admin$methodName <- 'initialize'
      private$..admin$state <- paste0("DataSourceQuanteda object instantiated.")
      private$..admin$modified <- Sys.time()
      private$..admin$created <- Sys.time()
      private$..admin$accessed <- Sys.time()
      private$..admin$logs <- LogR$new()

      invisible(self)
    }
  )
)

