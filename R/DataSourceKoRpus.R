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
    initialize = function(dataSource) {

      private$..dataSource <- dataSource
      private$..className <- 'DataSourceKoRpus'
      private$..methodName <- 'initialize'
      private$..state <- paste0("DataSourceKoRpus object instantiated.")
      private$..meta[["modified"]] <- Sys.time()
      private$..meta[["created"]] <- Sys.time()
      private$..meta[["accessed"]] <- Sys.time()
      private$..logs <- LogR$new()

      invisible(self)
    }
  )
)

