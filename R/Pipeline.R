#==============================================================================#
#                                 Pipeline                                     #
#==============================================================================#
#' Pipeline
#'
#' \code{Pipeline} Class containing the NLP processing, analysis, and modeling pipeline.
#'
#' The pipeline is comprised of six (6) stages
#' \enumerate{
#'  \item State 1: Data Preparation
#'  \item State 2: Data Processing
#'  \item State 3: Exploratory Data Analysis
#'  \item State 4: Feature Selection & Engineering
#'  \item State 5: Modeling
#'  \item State 6: Model Evaluation
#' }
#' The Pipeline object also contains general meta data for the pipeline as
#' well as the current stage of the pipeline.
#'
#' \strong{Pipeline Methods:}
#'  \describe{
#'   \item{\code{new(name, path)}}{Creates an object of Pipeline Class}
#'   \item{\code{getName()}}{Returns the name of the Pipeline object.}
#'   \item{\code{getPath()}}{Returns the path of the Pipeline object.}
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#'   \item{\code{logIt(level = 'Info', fieldName = NA)}}{Formats the log and calls the LogR class to log an event.}
#' }
#'
#' @param name A character string containing the name of the Pipeline object. This variable is used in the instantiation and remove methods.
#' @param path A chararacter string containing the relative path to the pipeline files.
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
    ..stages = list(),
    ..currentStage = numeric()
  ),

  active = list(
    currentStage = function(value) {
      if (missing(value)) {
        private$..currentStage
      } else {
        if ('Stage' %in% class(value)) {
          private$..currentStage <- value
          private$..admin$state <- paste0("Set current stage of ", private$..name,
                                          " pipeline to ", value$getStageName(), ".")
          self$logIt()
        } else {
          private$..admin$state <- paste0("Unable to set current stage of ", private$..name,
                                          " pipeline to ", value$getStageName(), ". ",
                                          value$getStageName(), " is not a valid Stage ",
                                          "object. ")
          self$logIt('Error')
          stop()
        }
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Pipeline Core Methods                           #
    #-------------------------------------------------------------------------#
    initialize = function(name, path) {

      # Instantiate variables
      private$..admin$className <- 'Pipeline'
      private$..admin$methodName <- 'initialize'
      private$..name <- name
      private$..admin$path <- path
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

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                          Aggregation Methods                            #
    #-------------------------------------------------------------------------#
    getStages = function() private$..stages,

    addStage = function(stage, current = FALSE) {

      private$..admin$methodName <- 'addStage'

      stageNum <- stage$getStageNum()
      stageName <- stage$getStageName()

      if (!is.null(private$..stages[[stageNum]])) {
        private$..admin$state <- paste0("Unable to add stage, ", stageName, ", ",
                                        "as it already exists. See ?",class(self)[[1]],
                                        " for further assistance.")
        self$logIt('Error')
        stop()
      }

      private$..stages[[stageNum]] <- stage
      if (current == TRUE) private$..currentStage <- stage
      private$..admin$state <- paste0("Added ", stageNum, " to Pipeline object, ",
                                      private$..name, ".")
      self$logIt()

      invisible(self)
    },

    removeStage = function(stage) {

      private$..admin$methodName <- 'removeStage'

      stageNum <- stage$getstageNum()
      stageName <- stage$getStageName()

      private$..stages[[stageNum]] <- NULL

      private$..admin$state <- paste0("Removed ", stageName, " from Pipeline object, ",
                                      private$..name, ".")
      self$logIt()

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                       Pipeline Execution Methods                        #
    #-------------------------------------------------------------------------#
    execute = function(restart = FALSE) {

      private$..admin$methodName <- 'execute'

      if (restart == TRUE) {
        stageNum <- 1
      } else {
        stageNum <- private$..currentStage
      }



    }


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
        name = private$..name,
        path = private$..admin$path,
        stages = private$..stages,
        state = private$..admin$state,
        modified = private$..admin$modified,
        created = private$..admin$created
      )

      return(pipeline)
    }
  )
)
