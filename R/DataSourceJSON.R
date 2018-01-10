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
    initialize = function(dataSource) {

      private$..dataSource <- dataSource
      private$..className <- 'DataSourceJSON'
      private$..methodName <- 'initialize'
      private$..state <- paste0("DataSourceJSON object instantiated.")
      private$..meta[["modified"]] <- Sys.time()
      private$..meta[["created"]] <- Sys.time()
      private$..meta[["accessed"]] <- Sys.time()
      private$..logs <- LogR$new()

      invisible(self)
    }
  )
)

