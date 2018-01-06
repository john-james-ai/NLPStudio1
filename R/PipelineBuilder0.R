#==============================================================================#
#                             PipelineBuilder0                                 #
#==============================================================================#
#' PipelineBuilder0
#'
#' \code{PipelineDirector} Abstract class defining the interface for building the Pipeline object.
#'
#' @section Pipeline Family of Classes Overview:
#' The Pipeline family of classes is an implementation of the builder design pattern,
#' as described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This builder pattern accommodates
#' the creation of different pipeline representations via separate
#' builder classes.#'
#'
#' \strong{Pipeline Family of Classes Participants:}
#' The Pipeline Family of classes is comprised of product classes, director classes, and abstract and concrete builder classes.
#'
#' The Product classes include:
#' \itemize{
#'  \item Pipeline: This class comprising the final pipeline 'product' class within the family.
#'  \item PipelineData: This class comprising the data pipeline within the family.
#'  \item PipelineFeatures: This class comprising the feature sets within the family.
#'  \item PipelineAnalyses: This class comprising the analysis objects created within the family.
#'  \item PipelineModel: This class comprising the model to be evaluated within the family.
#'  \item PipelineEval: This class comprising the model evaluation within the family.
#'  }
#'
#'  The Director classes include:
#' \itemize{
#'  \item PipelineDirector: Class responsible for building the Pipeline product via the concrete Builder classes.
#'  \item PipelineDirectorData: Class responsible for building the Data Pipeline via the concrete Builder classes.
#'  \item PipelineDirectorFeatures: Class responsible for building the Feature products via the concrete Builder classes.
#'  \item PipelineDirectorAnalyses: Class responsible for building the Analysis products via the concrete Builder classes.
#'  \item PipelineDirectorModel: Class responsible for building the Model products via the concrete Builder classes.
#'  \item PipelineDirectorEval: Class responsible for building the Model Evaluation products via the concrete Builder classes.
#'  }
#'
#'  The Abstract Builder classes include:
#' \itemize{
#'  \item PipelineBuilder0: Abstract class which defines the interface for concrete PipelineBuilder subclass.
#'  \item PipelineBuilderData0: Abstract class which defines the interface for concrete PipelineBuilderData subclasses.
#'  \item PipelineBuilderFeatures0: Abstract class which defines the interface for concrete PipelineBuilderFeatures subclasses.
#'  \item PipelineBuilderAnalyses0: Abstract class which defines the interface for concrete PipelineBuilderAnalyses subclasses.
#'  \item PipelineBuilderModel0: Abstract class which defines the interface for concrete PipelineBuilderModel subclasses.
#'  \item PipelineBuilderEval0: Abstract class which defines the interface for concrete PipelineBuilderEval subclasses.
#'  }
#'
#'  The Concrete Builder classes inclue:
#' \itemize{
#'  \item Pipeline #1: Hold-out cross validation strategy, analyzing, and modeling ngram based features for the Modified Kneser Ney (MKN) language model.
#'  \item Pipeline #2: Hold-out cross validation strategy, analyzing, and modeling ngram based features for the Katz language model.
#'  \item Pipeline #3: Hold-out cross validation strategy, analyzing, and modeling ngram and POS based features for the Combined language model.
#'  \item Pipeline #4: K-Fold  cross validation strategy, analyzing, and modeling ngram based features for the Modified Kneser Ney (MKN) language model.
#'  \item Pipeline #5: K-Fold  cross validation strategy, analyzing, and modeling ngram based features for the Katz language model.
#'  \item Pipeline #6: K-Fold  cross validation strategy, analyzing, and modeling ngram and POS based features for the Combined language model.
#' }
#'
#' @section PipelineBuilder0 Methods:
#'  \describe{
#'   \item{\code{new(name, path)}}{Not implemented for this abstract class.}
#'   \item{\code{buildData(data)}}{Not implemented for this abstract class.}
#'   \item{\code{buildFeatures(features)}}{Not implemented for this abstract class.}
#'   \item{\code{buildAnalysis(analysis)}}{Not implemented for this abstract class.}
#'   \item{\code{buildModel(model)}}{Not implemented for this abstract class.}
#'   \item{\code{buildEval(eval)}}{Not implemented for this abstract class.}
#'   \item{\code{getResult()}}{Not implemented for this abstract class.}
#'   \item{\code{accept(visitor)}}{Not implemented for this abstract class.}
#'   \item{\code{logIt(level = 'Info')}}{Not implemented for this abstract class. }
#' }
#'
#' @section Parameters:
#' @param name Character string representing the name of the Pipeline object.
#' @param path Character string representing the relative path in which the Pipeline object data will be stored.
#'
#' @docType class
#' @family Pipeline classes
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
PipelineBuilder0 <- R6::R6Class(
  classname = "PipelineBuilder0",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..builder = character()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                     Pipeline Initialization Method                      #
    #-------------------------------------------------------------------------#
    initialize = function(builder) {

      # Instantiate variables
      private$..builder <- builder

      private$..admin$className <- 'PipelineDirector'
      private$..admin$methodName <- 'initialize'
      private$..admin$state <- paste("PipelineDirector", name, "instantiated at", Sys.time())
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
    #                              Build Method                               #
    #-------------------------------------------------------------------------#
    build = function() {

      private$..admin$methodName <- 'build'
      private$..builder$buildDataExternal()
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
