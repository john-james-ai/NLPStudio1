#==============================================================================#
#                             PipelineDirectorData                             #
#==============================================================================#
#' PipelineDirectorData
#'
#' \code{PipelineDirectorData} Class that constructs the data pipeline through a builder class.
#'
#' Class directs the construction of the data pipeline which includes:
#' \itemize{
#'  \item Raw Data: The immutable raw data u
#'  \item Repaired Data: Data encoding errors corrected
#'  \item CV: Data which has been split in accordance with hold-out or k-fold cross-validation methods.
#'  \item Processed: Processed training, validation and test sets.
#' }
#' For a full description of the Pipeline family of classes and its participants,
#' @seealso \code{\link{Pipeline}}
#'
#' @section PipelineDirectorData Methods:
#'  \describe{
#'   \item{\code{new(builder)}}{Creates a document meta data field.}
#'   \item{\code{corpusMeta(field)}}{Creates a corpus meta data field.}
#'   }
#'
#' @section PipelineDirectorData Parameters:
#' @param builder Concrete data builder object
#' @param dataSource DataSource class object
#' @param name Character string indicating the name of
#'
#' @docType class
#' @family Pipeline classes
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
PipelineDirectorData <- R6::R6Class(
  classname = "PipelineDirectorData",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..builder = character(),
    ..dataSource = character(),
    ..names = list(),
    ..paths = list()
  ),

  active = list(
    rawName = function(value) {
      if (missing(value)) {
        private$..names[['raw']]
      } else {
        private$..names[['raw']] <- value
      }
    },

    rawPath = function(value) {
      if (missing(value)) {
        private$..paths[['raw']]
      } else {
        private$..paths[['raw']] <- value
      }
    },

  )

  public = list(

    #-------------------------------------------------------------------------#
    #                     Pipeline Initialization Method                      #
    #-------------------------------------------------------------------------#
    initialize = function(name, builder, dataSource) {

      # Instantiate variables
      private$..builder <- builder
      private$..dataSource <- dataSource

      private$..admin$className <- 'PipelineDirectorData'
      private$..admin$methodName <- 'initialize'
      private$..admin$state <- paste("PipelineDirectorData", name, "instantiated at", Sys.time())
      private$..admin$logs <- LogR$new()
      private$..admin$modified <- Sys.time()
      private$..admin$created <- Sys.time()
      private$..admin$accessed <- Sys.time()

      # Validate Pipeline
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..admin$state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Create log entry
      self$logIt()

      invisible(self)
    },

    getBuilder = function() private$..builder,
    getDataSource = function() private$..dataSource,

    #-------------------------------------------------------------------------#
    #                              Build Method                               #
    #-------------------------------------------------------------------------#
    build = function() {

      private$..admin$methodName <- 'build'
      private$..builder$buildDataRaw()
      private$..builder$buildDataPrepared()
      private$..builder$buildDataCvCorpora()
      private$..builder$buildDataProcessed()
      private$..builder$buildFeatureNGram()
      private$..builder$buildFeaturePOS()
      private$..builder$buildAnalysis()
      private$..builder$buildModel()
      private$..builder$buildEvaluation()

      private$..admin$state <- paste0('Completed build of ',
                                      private$..builder$getName(), ". ")

      self$logIt()

      return(private$..builder$getResult())
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$pipeline(self)
    },

    #-------------------------------------------------------------------------#
    #                           Test Methods                                  #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      #TODO: Remove after testing

      director = list(
        className = private$..admin$className,
        methodName = private$..admin$methodName,
        builder = private$..builder,
        state = private$..admin$state,
        modified = private$..admin$modified,
        created = private$..admin$created,
        accessed = private$..admin$accessed
      )

      return(director)
    }
  )
)
