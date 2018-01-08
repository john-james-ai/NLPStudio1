#==============================================================================#
#                               DataSourceJSON                                 #
#==============================================================================#
#' DataSourceJSON
#'
#' \code{DataSourceJSON} Concrete class for defining JSON data sources.
#'
#' @template dataSourceClasses
#' @template  dataSourceMethods
#' @template  dataSourceParams
#'
#' @return dataSource: An object of one of the DataSourceJSON class.
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Source data classes
#' @export
DataSourceJSON <- R6::R6Class(
  "DataSourceJSON",
  inherit = DataSource0,

  public = list(
    initialize = function(name, dataSource) {

      private$..name <- name
      private$..dataSource <- dataSource
      private$..admin$className <- 'DataSourceJSON'
      private$..admin$methodName <- 'initialize'
      private$..admin$state <- paste0("DataSourceJSON object instantiated.")
      private$..admin$modified <- Sys.time()
      private$..admin$created <- Sys.time()
      private$..admin$accessed <- Sys.time()
      private$..admin$logs <- LogR$new()

      invisible(self)
    }
  )
)

