#' @section Pipeline Family of Classes Introduction:
#' The Pipeline family of classes is an implementation of the builder design pattern,
#' as described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This builder pattern accommodates
#' the creation of different pipeline representations via separate
#' builder classes.
#'
#' \strong{Pipeline Family of Classes Organization:}
#' The Pipeline Family of classes is organized by pipeline scope, then by
#' role or behavior of each of the participants.
#'
#' \emph{Pipeline Groups by Scope}:
#' The pipeline groups by scope are:
#' \itemize{
#'  \item Group 1: Pipelines that have 'end-to-end' scope from data acquisition,
#'  through processing,  analysis, modeling and evaluation.
#'  \item Group 2: Pipelines responsible for building the conical data used for
#'  training and modeling.
#'  \item Group 3: Pipelines that produce the features used for training
#'  and modeling.
#'  \item Group 4: Pipelines that render the analysis objects
#'  \item Group 5: Pipelines that build the models
#'  \item Group 6: Pipelines responsible for model evaluation.
#'  }
#'
#' \emph{Pipeline Participants by Role}:
#' The pipelines in each of the above groups are constructed through the
#' collaboration of three types of classes.
#' \itemize{
#'  \item Abstract Builder: Classes specify an abstract interface for creating
#'  the pipeline, component-by-component. These classes are designated with a
#'  trailing zero in the class name.
#'  \item Concrete Builder: Classes that construct and assemble the parts of
#'  the pipeline by implementing the Abstract Builder interface.  Individual Concrete
#'  Builders are responsible for defining and configuring an specific representation
#'  of the pipeline object.
#'  \item Director: The pipeline director constructs the pipeline object
#'  using the builder interface. The director is also responsible for
#'  setting (designing) pipeline parameters and passing those parameters
#'  to the individual Concrete Builders.
#'  }
#'
#' \strong{Pipeline Participants Group 1: End-to-End Pipelines}
#'
#' \emph{Group 1 Pipeline Products}:
#' The following pipeline 'products' include data acquisition, processing,
#' feature selection, analysis, modeling and evaluation.
#' \itemize{
#'  \item PipelineMKN1: Modified Kneser Ney (MKN) Ngram based language model, with hold-out cross-validation.
#'  \item PipelineMKN2: Modified Kneser Ney (MKN) Ngram based language model, with K-Fold cross-validation.
#'  \item PipelineKBO1: Katz Back-Off (KBO) Ngram based language model, with hold-out cross-validation.
#'  \item PipelineKBO2: Katz Back-Off (KBO) Ngram based language model, with K-Fold cross-validation.
#'  \item PipelineCombined1: Combined Ngram/POS based language model, with hold-out cross-validation.
#'  \item PipelineCombined2: Combined Ngram/POS based language model, with K-Fold cross-validation.
#'  }
#'
#' \emph{Group 1 Pipeline Participants}:
#' The group 1 pipelines are constructed by the following participants:
#' \itemize{
#'  \item PipelineDirector: Class responsible for building the Pipeline product via the concrete Builder classes.
#'  \item PipelineBuilder0: Abstract class which defines the interface for concrete PipelineBuilder subclass.
#'  \item PipelineBuilderMKN1: Builder for the  Modified Kneser Ney (MKN) Ngram based language model, with hold-out cross-validation.
#'  \item PipelineBuilderMKN2: Builder for the  Modified Kneser Ney (MKN) Ngram based language model, with K-Fold cross-validation.
#'  \item PipelineBuilderKBO1: Builder for the  Katz Back-Off (KBO) Ngram based language model, with hold-out cross-validation.
#'  \item PipelineBuilderKBO2: Builder for the  Katz Back-Off (KBO) Ngram based language model, with K-Fold cross-validation.
#'  \item PipelineBuilderCombined1: Builder for the  Combined Ngram/POS based language model, with hold-out cross-validation.
#'  \item PipelineBuilderCombined2: Builder for the  Combined Ngram/POS based language model, with K-Fold cross-validation.
#'  }
#'  Note that there is a concrete builder for each of the final pipeline products in Group 1.
#'
#'
#'
#' \strong{Pipeline Participants Group 2: Data Pipelines}
#'
#' \emph{Group 2 Pipeline Products}:
#' The following pipeline 'products' include the raw, preprocessed, and processed cross-validation sets
#' used for model training and evaluation.
#' \itemize{
#'  \item PipelineDataHoldOut: Data for hold-out cross-validation method.
#'  \item PipelineDataKFold: Data for K-Fold cross-validation method.
#'  }
#'
#' \emph{Group 2 Pipeline Participants}:
#' The group 2 data pipelines are constructed by the following participants:
#' \itemize{
#'  \item PipelineDirectorData: Class responsible for building the Data Pipeline via the concrete Builder classes.
#'  \item PipelineBuilderData0: Abstract class which defines the interface for concrete PipelineBuilderData subclasses.
#'  \item PipelineBuilderDataHoldOut: Data for hold-out cross-validation method.
#'  \item PipelineBuilderDataKFold: Data for K-Fold cross-validation method.
#'  }
#'
#'
#'
#' \strong{Pipeline Participants Group 3: Feature Pipelines}
#'
#' \emph{Group 3 Pipeline Products}:
#' The following pipeline 'products' include the features and transformed data used
#' during model training.
#' \itemize{
#'  \item PipelineFeaturesNGrams: Unigrams, bigrams, trigrams, quadgrams, and quintgrams.
#'  \item PipelineFeaturesNGramsPOS: Unigrams, bigrams, trigrams, quadgrams, and quintgrams and POS tags
#'  }
#'
#' \emph{Group 3 Pipeline Participants}:
#' The group 3 feature pipelines are constructed by the following participants:
#' \itemize{
#'  \item PipelineDirectorFeatures: Class responsible for building the Data Pipeline via the concrete Builder classes.
#'  \item PipelineBuilderFeatures0: Abstract class which defines the interface for concrete PipelineBuilderData subclasses.
#'  \item PipelineBuilderFeaturesNGrams: Unigrams, bigrams, trigrams, quadgrams, and quintgrams.
#'  \item PipelineBuilderFeaturesNGramsPOS: Unigrams, bigrams, trigrams, quadgrams, and quintgrams and POS tags
#'  }
#'
#'
#'
#' \strong{Pipeline Participants Group 4: Analysis Pipelines}
#'
#' \emph{Group 4 Pipeline Products}:
#' The following pipeline 'products' include the analyses conducted on the data and
#' the features.
#' \itemize{
#'  \item PipelineAnalysesNGrams: Analyses of unigrams, bigrams, trigrams, quadgrams, and quintgrams.
#'  \item PipelineAnalysesNGramsPOS: Analyses of unigrams, bigrams, trigrams, quadgrams, and quintgrams and POS tags
#'  }
#'
#' \emph{Group 4 Pipeline Participants}:
#' The group 4 analysis pipelines are constructed by the following participants:
#' \itemize{
#'  \item PipelineDirectorAnalyses: Class responsible for building the Data Pipeline via the concrete Builder classes.
#'  \item PipelineBuilderAnalyses0: Abstract class which defines the interface for concrete PipelineBuilderData subclasses.
#'  \item PipelineBuilderAnalysesNGrams: Analyses of unigrams, bigrams, trigrams, quadgrams, and quintgrams.
#'  \item PipelineBuilderAnalysesNGramsPOS: Analyses of unigrams, bigrams, trigrams, quadgrams, and quintgrams and POS tags
#'  }
#'
#'
#'
#' \strong{Pipeline Participants Group 5: Model Pipelines}
#'
#' \emph{Group 5 Pipeline Products}:
#' The following pipeline 'products' include the production of the language
#' models evaluated in this package.
#' \itemize{
#'  \item PipelineModelMKN: Modified Kneser Ney Model.
#'  \item PipelineModelKatz: Katz Back-off Model.
#'  \item PipelineModelCombined: Combined Model.
#'  }
#'
#' \emph{Group 5 Pipeline Participants}:
#' The group 5 model pipelines are constructed by the following participants:
#' \itemize{
#'  \item PipelineDirectorModel: Class responsible for building the Model products via the concrete Builder classes.
#'  \item PipelineBuilderModel0: Abstract class which defines the interface for concrete PipelineBuilderModel subclasses.
#'  \item PipelineBuilderModelMKN: Builder for Modified Kneser Ney Model.
#'  \item PipelineBuilderModelKatz: Builder for Katz Back-off Model.
#'  \item PipelineBuilderModelCombined: Builder for Combined Model.
#'  }
#'
#'
#'
#' \strong{Pipeline Participants Group 6: Model Evaluation Pipelines}
#'
#' \emph{Group 6 Pipeline Products}:
#' The following pipeline 'products' include the model evaluations for each of
#' the models evaluated in this package.
#' \itemize{
#'  \item PipelineEvalMKN: Evaluation of Modified Kneser Ney Model.
#'  \item PipelineEvalKatz: Evaluation of Katz Back-off Model.
#'  \item PipelineEvalCombined: Evaluation of Combined
#'  }
#'
#' \emph{Group 6 Pipeline Participants}:
#' The group 6 model evaluation pipelines are constructed by the following participants:
#' \itemize{
#'  \item PipelineDirectorEval: Class responsible for building the Model Evaluation products via the concrete Builder classes.
#'  \item PipelineBuilderEval0: Abstract class which defines the interface for concrete PipelineBuilderEval subclasses.
#'  \item PipelineBuilderEvalMKN: Evaluation of Modified Kneser Ney Model.
#'  \item PipelineBuilderEvalKatz: Evaluation of Katz Back-off Model.
#'  \item PipelineBuilderEvalCombined: Evaluation of Combined Model.
#'  }
