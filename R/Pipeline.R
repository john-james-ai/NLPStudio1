#==============================================================================#
#                                 Pipeline                                     #
#==============================================================================#
#' Pipeline
#'
#' \code{Pipeline} Class containing the NLP processing, analysis, and modeling pipeline.
#'
#' The pipeline starts with the extraction of data from external sources and includes
#' the establishment of the raw data sets, the cross-validation sets, data
#' preprocessing, analysis, feature engineering and selection, modeling
#' an model evaluation.
#'
#' \strong{Pipeline Methods:}
#'  \describe{
#'   \item{\code{new(name, desc = NULL)}}{Creates an object of Pipeline Class}
#'   \item{\code{desc}}{A getter/setter method allowing clients to retrieve and set the Pipeline description variable.}
#'   \item{\code{getName()}}{Returns the name of the Pipeline object.}
#'   \item{\code{getPath()}}{Returns the path of the Pipeline object.}
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#'   \item{\code{logIt(level = 'Info', fieldName = NA)}}{Formats the log and calls the LogR class to log an event.}
#' }
#'
#' @param name A character string containing the name of the Pipeline object. This variable is used in the instantiation and remove methods.
#' @param desc A chararacter string containing the description of the Pipeline
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Pipeline <- R6::R6Class(
  classname = "Pipeline",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..data = list(),
    ..analyis = list(),
    ..features = list(),
    ..models = list(),
    ..eval = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Pipeline Core Methods                           #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL) {

      # Instantiate variables
      private$..admin$className <- 'Pipeline'
      private$..admin$methodName <- 'initialize'
      private$..admin$name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "pipeline"), desc)
      private$..admin$path <- file.path("./NLPStudio/pipelines", name)
      private$..parent <- NLPStudio$new()$getInstance()
      private$..admin$state <- paste("Pipeline", name, "instantiated at", Sys.time())
      private$..admin$logs <- LogR$new()
      private$..admin$modified <- Sys.time()
      private$..admin$created <- Sys.time()

      # Validate Pipeline
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..admin$state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Create directory
      dir.create(private$..admin$path, recursive = TRUE)

      # Create log entry
      self$logIt()

      # Assign its name in the global environment
      assign(name, self, envir = .GlobalEnv)

      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      name <- visitor$getName()
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
        name = private$..admin$name,
        desc = private$..desc,
        path = private$..admin$path,
        parent = private$..parent,
        data = private$..data,
        analysis = private$..analysis,
        features = private$..features,
        models = private$..models,
        eval = private$..eval,
        logs <- private$..admin$logs,
        state = private$..admin$state,
        modified = private$..admin$modified,
        created = private$..admin$created
      )

      return(pipeline)
    }
  )
)
