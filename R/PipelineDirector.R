#==============================================================================#
#                             PipelineDirector                                 #
#==============================================================================#
#' PipelineDirector
#'
#' \code{PipelineDirector} Class that constructs the Pipeline object using a concrete builder class.
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
#' The Pipeline Family of classes is comprised of:
#' \itemize{
#'  \item Pipeline: This class comprising the 'product' class within the family.
#'  \item PipelineBuilder0: Abstact class, defining the base operations and interface for the concrete builder classes.
#'  \item PipelineBuilderWebSourceMKN: Concrete builder class for web source corpora and the Modified Kneser Ney language model.
#'  \item PipelineBuilderWebSourceKFoldMKN: Concrete builder class for web source corpora, using K-Fold validation and the Modified Kneser Ney language model.
#'  \item PipelineBuilderWebSourceKatz: Concrete builder class for web source corpora and the Katz back-off language model.
#'  \item PipelineBuilderWebSourceKFoldKatz: Concrete builder class for web source corpora, using K-Fold validation and the Katz back-off language model.
#'  \item PipelineDirector: Constructs the Pipeline object using a concrete builder class.
#' }
#'
#' \strong{PipelineDirector Methods:}
#'  \describe{
#'   \item{\code{new(builder)}}{Creates an object of PipelineDirector Class}
#'   \item{\code{build()}}{Builds the Pipeline object via the concrete builder parameter.}
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#'   \item{\code{logIt(level = 'Info', fieldName = NA)}}{Formats the log and calls the LogR class to log an event.}
#' }
#'
#' @param builder A concrete Pipeline builder object
#'
#' @docType class
#' @family Pipeline classes
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
PipelineDirector <- R6::R6Class(
  classname = "PipelineDirector",
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
