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
    initialize = function(dataSource) {

      private$..dataSource <- dataSource
      private$..className <- 'DataSourceText'
      private$..methodName <- 'initialize'
      private$..state <- paste0("DataSourceText object instantiated.")
      private$..meta[["modified"]] <- Sys.time()
      private$..meta[["created"]] <- Sys.time()
      private$..meta[["accessed"]] <- Sys.time()
      private$..logs <- LogR$new()

      invisible(self)
    }
  )
)

