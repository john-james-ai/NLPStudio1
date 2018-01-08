#==============================================================================#
#                               DataSourceKoRpus                               #
#==============================================================================#
#' DataSourceKoRpus
#'
#' \code{DataSourceKoRpus} Concrete class for defining KoRpus data sources.
#'
#' @template dataSourceClasses
#' @template  dataSourceMethods
#' @template  dataSourceParams
#'
#' @return dataSource: An object of one of the DataSourceKoRpus class.
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Source data classes
#' @export
DataSourceKoRpus <- R6::R6Class(
  "DataSourceKoRpus",
  inherit = DataSource0,

  public = list(
    initialize = function(name, dataSource) {

      private$..name <- name
      private$..dataSource <- dataSource
      private$..admin$className <- 'DataSourceKoRpus'
      private$..admin$methodName <- 'initialize'
      private$..admin$state <- paste0("DataSourceKoRpus object instantiated.")
      private$..admin$modified <- Sys.time()
      private$..admin$created <- Sys.time()
      private$..admin$accessed <- Sys.time()
      private$..admin$logs <- LogR$new()

      invisible(self)
    }
  )
)

