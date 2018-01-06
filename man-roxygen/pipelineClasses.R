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
#'  \itemPipelineAlpha: Modified Kneser Ney (MKN) Ngram based language model, with hold-out cross-validation.
#'  \itemPipelineBeta: Modified Kneser Ney (MKN) Ngram based language model, with K-Fold cross-validation.
#'  \itemPipelineGamma: Katz Back-Off (KBO) Ngram based language model, with hold-out cross-validation.
#'  \itemPipelineDelta: Katz Back-Off (KBO) Ngram based language model, with K-Fold cross-validation.
#'  \itemPipelineEpsilon: Combined Ngram/POS based language model, with hold-out cross-validation.
#'  \itemPipelineZeta: Combined Ngram/POS based language model, with K-Fold cross-validation.
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
#'  The Concrete Builder classes include:
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
