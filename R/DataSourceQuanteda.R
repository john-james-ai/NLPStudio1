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
    initialize = function(dataSource) {

      private$..dataSource <- dataSource
      private$..className <- 'DataSourceQuanteda'
      private$..methodName <- 'initialize'
      private$..state <- paste0("DataSourceQuanteda object instantiated.")
      private$..meta[["modified"]] <- Sys.time()
      private$..meta[["created"]] <- Sys.time()
      private$..meta[["accessed"]] <- Sys.time()
      private$..logs <- LogR$new()

      invisible(self)
    }
  )
)

