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
    initialize = function(dataSource) {

      private$..dataSource <- dataSource
      private$..className <- 'DataSourceTM'
      private$..methodName <- 'initialize'
      private$..state <- paste0("DataSourceTM object instantiated.")
      private$..meta[["modified"]] <- Sys.time()
      private$..meta[["created"]] <- Sys.time()
      private$..meta[["accessed"]] <- Sys.time()
      private$..logs <- LogR$new()

      invisible(self)
    }
  )
)

