#==============================================================================#
#                               DataSourceText                                 #
#==============================================================================#
#' DataSourceText
#'
#' \code{DataSourceText} Concrete class for defining text data sources.
#'
#' @template dataSourceClasses
#' @template  dataSourceMethods
#' @template  dataSourceParams
#'
#' @return dataSource: An object of one of the DataSourceText class.
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Source data classes
#' @export
DataSourceText <- R6::R6Class(
  "DataSourceText",
  inherit = DataSource0,

  public = list(
    initialize = function(name, dataSource) {

      private$..name <- name
      private$..dataSource <- dataSource
      private$..admin$className <- 'DataSourceText'
      private$..admin$methodName <- 'initialize'
      private$..admin$state <- paste0("DataSourceText object instantiated.")
      private$..admin$modified <- Sys.time()
      private$..admin$created <- Sys.time()
      private$..admin$accessed <- Sys.time()
      private$..admin$logs <- LogR$new()

      invisible(self)
    }
  )
)

