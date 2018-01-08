#==============================================================================#
#                               DataSourceTM                                   #
#==============================================================================#
#' DataSourceTM
#'
#' \code{DataSourceTM} Concrete class for defining TM data sources.
#'
#' @template dataSourceClasses
#' @template  dataSourceMethods
#' @template  dataSourceParams
#'
#' @return dataSource: An object of one of the DataSourceTM class.
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Source data classes
#' @export
DataSourceTM <- R6::R6Class(
  "DataSourceTM",
  inherit = DataSource0,

  public = list(
    initialize = function(name, dataSource) {

      private$..name <- name
      private$..dataSource <- dataSource
      private$..admin$className <- 'DataSourceTM'
      private$..admin$methodName <- 'initialize'
      private$..admin$state <- paste0("DataSourceTM object instantiated.")
      private$..admin$modified <- Sys.time()
      private$..admin$created <- Sys.time()
      private$..admin$accessed <- Sys.time()
      private$..admin$logs <- LogR$new()

      invisible(self)
    }
  )
)

