#==============================================================================#
#                                 Pipeline                                     #
#==============================================================================#
#' Pipeline
#'
#' \code{Pipeline} Class containing the NLP processing, analysis, and modeling pipeline.
#'
#' @template pipelineClasses
#'
#' @section Pipeline Methods:
#'  \describe{
#'   \item{\code{new(...)}}{Creates an object of Pipeline Class}
#'   \item{\code{getName()}}{Returns the name of the Pipeline object.}
#'   \item{\code{getPath()}}{Returns the path of the Pipeline object.}
#'   \item{\code{getData()}}{Returns the Pipeline data objects. }
#'   \item{\code{getFeatures()}}{Returns the Pipeline feature objects. }
#'   \item{\code{getAnalyses()}}{Returns the Pipeline analysis objects. }
#'   \item{\code{getModel()}}{Returns the Pipeline model objects. }
#'   \item{\code{getEval()}}{Returns the Pipeline model evaluation objects. }
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#'   \item{\code{logIt(level = 'Info')}}{Formats the log and calls the LogR class to log an event.}
#' }
#'
#' @section Pipeline Parameters:
#' @param name A character string containing the name of the Pipeline object.
#' @param path A chararacter string containing the relative path to the pipeline files.
#' @param data PipelineData object, containing external, raw, cross-validation sets and preprocessed data.
#' @param features PipelineFeatures object.
#' @param analyses PipelineAnalyses object.
#' @param model PipelineModel object.
#' @param eval PipelineEval object.
#'
#' @docType class
#' @family Pipeline classes
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Pipeline <- R6::R6Class(
  classname = "Pipeline",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..data = list(),
    ..features = list(),
    ..analyses = list(),
    ..model = character(),
    ..eval = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                     Pipeline Initialization Method                      #
    #-------------------------------------------------------------------------#
    initialize = function(name, path, data, features, analyses, model, eval) {

      # Instantiate variables
      private$..name <- name
      private$..path <- path
      private$..data <- data
      private$..features <- features
      private$..analyses <- analyses
      private$..model <- model
      private$..eval <- eval

      private$..admin$className <- 'Pipeline'
      private$..admin$methodName <- 'initialize'
      private$..admin$state <- paste("Pipeline", name, "instantiated at", Sys.time())
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

    #-------------------------------------------------------------------------#
    #                            Getter Methods                               #
    #-------------------------------------------------------------------------#
    getData = function() private$..data,
    getFeatures = function() private$..features,
    getAnalyses = function() private$..analyses,
    getModel = function() private$..model,
    getEval = function() private$..eval,

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

      pipeline = list(
        className = private$..admin$className,
        methodName = private$..admin$methodName,
        name = private$..name,
        path = private$..path,
        data = private$..data,
        features = private$..features,
        analyses = private$..analyses,
        model = private$..model,
        eval = private$..eval,
        state = private$..admin$state,
        modified = private$..admin$modified,
        created = private$..admin$created,
        accessed = private$..admin$accessed
      )

      return(pipeline)
    }
  )
)
