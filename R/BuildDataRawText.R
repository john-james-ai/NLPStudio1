#==============================================================================#
#                          BuildDataRawTextSource                                  #
#==============================================================================#
#' BuildDataRawTextSource
#'
#' \code{BuildDataRawTextSource} Concrete class for building raw Corpus objects from text sources.
#'
#' @template buildDataRawClasses
#' @template buildDataRawMethods
#' @template buildDataRawParams
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Build raw data classes
#' @export
BuildDataRawTextSource <- R6::R6Class(
  "BuildDataRawTextSource",
  inherit = BuildDataRaw0,

  public = list(
    initialize = function(pipeline, dataSource) {

      private$..path <- path
      private$..admin <- 'BuildDataRawTextSource'
      private$..admin <- 'initialize'
      private$..admin <- paste0("BuildDataRawTextSource object instantiated.")
      private$..admin <- Sys.time()
      private$..admin <- Sys.time()
      private$..admin <- Sys.time()
      private$..admin <- LogR()

      invisible(self)
    },
    execute = function() {

    }
  )
)

