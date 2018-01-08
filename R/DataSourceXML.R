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
    initialize = function(name, dataSource) {

      private$..name <- name
      private$..dataSource <- dataSource
      private$..admin$className <- 'DataSourceXML'
      private$..admin$methodName <- 'initialize'
      private$..admin$state <- paste0("DataSourceXML object instantiated.")
      private$..admin$modified <- Sys.time()
      private$..admin$created <- Sys.time()
      private$..admin$accessed <- Sys.time()
      private$..admin$logs <- LogR$new()

      invisible(self)
    }
  )
)

