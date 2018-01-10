#==============================================================================#
#                               DataSourceXML                                  #
#==============================================================================#
#' DataSourceXML
#'
#' \code{DataSourceXML} Concrete class for defining XML data sources.
#'
#' @template dataSourceClasses
#' @template  dataSourceMethods
#' @template  dataSourceParams
#'
#' @return dataSource: An object of one of the DataSourceXML class.
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Source data classes
#' @export
DataSourceXML <- R6::R6Class(
  "DataSourceXML",
  inherit = DataSource0,

  public = list(
    initialize = function(dataSource) {

      private$..dataSource <- dataSource
      private$..className <- 'DataSourceXML'
      private$..methodName <- 'initialize'
      private$..state <- paste0("DataSourceXML object instantiated.")
      private$..meta[["modified"]] <- Sys.time()
      private$..meta[["created"]] <- Sys.time()
      private$..meta[["accessed"]] <- Sys.time()
      private$..logs <- LogR$new()

      invisible(self)
    }
  )
)

