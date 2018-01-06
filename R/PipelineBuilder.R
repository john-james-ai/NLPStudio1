#==============================================================================#
#                             PipelineBuilder                                  #
#==============================================================================#
#' PipelineBuilder
#'
#' \code{PipelineBuilder} Concrete builder class for the pipeline object.
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
#'  The Concrete Data Builder classes inclue:
#' \itemize{
#'  \item PipelineBuilder: Builder for the full pipeline
#'  \item PipelineBuilderDataHoldOut: Data for hold-out cross-validation method.
#'  \item PipelineBuilderDataKFold: Data for K-Fold cross-validation method.
#'  \item PipelineBuilderFeaturesNGrams: Unigrams, bigrams, trigrams, quadgrams, and quintgrams.
#'  \item PipelineBuilderFeaturesNGramsPOS: Unigrams, bigrams, trigrams, quadgrams, and quintgrams and POS tags
#'  \item PipelineBuilderAnalysesNGrams: Analyses of unigrams, bigrams, trigrams, quadgrams, and quintgrams.
#'  \item PipelineBuilderAnalysesNGramsPOS: Analyses of unigrams, bigrams, trigrams, quadgrams, and quintgrams and POS tags
#'  \item PipelineBuilderModelMKN: Modified Kneser Ney Model.
#'  \item PipelineBuilderModelKatz: Katz Back-off Model.
#'  \item PipelineBuilderModelCombined: Combined Model.
#'  \item PipelineBuilderEvalMKN: Evaluation of Modified Kneser Ney Model.
#'  \item PipelineBuilderEvalKatz: Evaluation of Katz Back-off Model.
#'  \item PipelineBuilderEvalCombined: Evaluation of Combined Model.
#' }
#'
#' @section PipelineBuilder Methods:
#'  \describe{
#'   \item{\code{new(name, path)}}{Not implemented for this abstract class.}
#'   \item{\code{buildData(data)}}{Not implemented for this abstract class.}
#'   \item{\code{buildFeatures(features)}}{Not implemented for this abstract class.}
#'   \item{\code{buildAnalyses(analyses)}}{Not implemented for this abstract class.}
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
PipelineBuilder <- R6::R6Class(
  classname = "PipelineBuilder",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..data = list(),
    ..features = list(),
    ..analyses = list(),
    ..model = list(),
    ..eval = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                     Pipeline Initialization Method                      #
    #-------------------------------------------------------------------------#
    initialize = function(name, path) {

      private$..name <- name
      private$..path <- path
      private$..admin$className <- "PipelineBuilder"
      private$..admin$methodName <- "initialize"
      private$..admin$state <- paste0("PipelineBuilder instantiated.")
      private$..admin$logs <- LogR$new()
      private$..admin$modified <- Sys.time()
      private$..admin$created <- Sys.time()
      private$..admin$accessed <- Sys.time()

      # Validate Builder
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..admin$state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      self$logIt()
      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$pipelineBuilder(self)
    },

    #-------------------------------------------------------------------------#
    #                           Test Methods                                  #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      #TODO: Remove after testing

      builder = list(
        className = private$..admin$className,
        methodName = private$..admin$methodName,
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

      return(builder)
    }
  )
)
