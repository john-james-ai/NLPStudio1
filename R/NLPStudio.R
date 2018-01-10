## ---- NLPStudio
#==============================================================================#
#                                 NLPStudio                                    #
#==============================================================================#
#' NLPStudio
#'
#' \code{NLPStudio} Containing class containing NLP data and modeling pipelines.
#'
#' This class creates and manages data science pipelines. Multiple pipelines
#' can be created, with separate data sets, feature engineering, and models.
#'
#' \strong{NLPStudio Core Methods:}
#' \describe{
#'  \item{\code{new()}}{Initializes the NLPStudio as s singleton object at load time.}
#'
#'  \item{\code{getInstance()}}{Returns the current NLPStudio instance object. This will be the only instantiation called "nlpStudio.},
#' }
#'
#'
#'
#' @docType class
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
NLPStudio <- R6::R6Class(
  "SingletonContainer",
  portable = FALSE,
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Singleton,
  public = list(
    initialize = function(...) {
      Class <<- R6::R6Class(
        classname = "NLPStudio",
        private = list(
          ..className = 'NLPStudio',
          ..methodName = character(),
          ..name = character(),
          ..desc = character(),
          ..path = character(),
          ..pipelines = list(),
          ..logs = character(),
          ..state = character(),
          ..created = "None",
          ..modified = "None"
        ),

        public = list(
          #-------------------------------------------------------------------#
          #                       NLPStudio Methods                           #
          #-------------------------------------------------------------------#
          initialize = function() {

            # Create single instance of NLPStudio object
            private$..className <- 'NLPStudio'
            private$..methodName <- 'initialize'
            private$..meta[["name"]] <- "nlpStudio"
            private$..desc <- "NLPStudio: Natural Language Processing Environment"
            private$..path <- "./NLPStudio"
            private$..meta[["modified"]] <- Sys.time()
            private$..meta[["created"]] <- Sys.time()

            # Create NLPStudio home directory
            if (!dir.exists(private$..path)) dir.create(private$..path, recursive = TRUE)

            # # Create logger and initialization log entry
            private$..logs <- LogR$new()
            private$..state <- paste0("Initialized NLPStudio.")
            self$logIt()

            # Assign its name in the global environment
            assign(private$..meta[["name"]], self, envir = .GlobalEnv)
            invisible(self)
          },

          getInstance = function()  {
            invisible(self)
          },

          #-------------------------------------------------------------------------#
          #                             Basic Getters                               #
          #-------------------------------------------------------------------------#
          getName = function() private$..meta[["name"]],
          getClassName = function() private$..className,
          getPath = function() private$..path,

          #-------------------------------------------------------------------------#
          #                           Composite Methods                             #
          #-------------------------------------------------------------------------#

          getPipelines = function() private$..pipelines,

          addPipeline = function(pipeline) {

            # Update current method
            private$..methodName <- 'addPipeline'

            # Validation
            v <- Validator$new()
            status <- v$addChild(self, pipeline)
            if (status[['code']] == FALSE) {
              private$..state <- status[['msg']]
              self$logIt(level = 'Error')
              stop()
            }
            # Get pipeline name
            pipelineName <- pipeline$getName()

            # Add pipeline to list of pipelines
            private$..pipelines[[pipelineName]] <- pipeline

            # Set parent to nlppipelines
            pipeline$parent <- self

            # Update modified time
            private$..meta[["modified"]] <- Sys.time()

            # Save state and log Event
            private$..state <-
              paste("Pipeline", pipelineName, "added to nlpPipelines at", Sys.time())
            self$logIt()

            # Assign its name in the global environment
            assign(private$..meta[["name"]], self, envir = .GlobalEnv)
            invisible(self)

          },

          removePipeline = function(pipeline) {
            #TODO: Archive to archive folder then remove

            # Update current method
            private$..methodName <- 'removePipeline'

            # Validation
            v <- Validator$new()
            status <- v$removeChild(self, document)
            if (status[['code']] == FALSE) {
              private$..state <- status[['msg']]
              self$logIt(level = 'Error')
              stop()
            }

            name <- pipeline$getName()
            if (!is.null(private$..pipelines[[name]]))  private..pipelines[[name]] <- NULL

            # Assign its name in the global environment
            assign(private$..meta[["name"]], self, envir = .GlobalEnv)
            invisible(self)

          },

          #-------------------------------------------------------------------------#
          #                            Log Method                                   #
          #-------------------------------------------------------------------------#
          logIt = function(level = 'Info', fieldName = NA) {

            private$..logs$entry$owner <- private$..meta[["name"]]
            private$..logs$entry$className <- private$..className
            private$..logs$entry$methodName <- private$..methodName
            private$..logs$entry$level <- level
            private$..logs$entry$msg <- private$..state
            private$..logs$entry$fieldName <- fieldName
            private$..logs$created <- Sys.time()
            private$..logs$writeLog()
          },

          #-------------------------------------------------------------------------#
          #                           Visitor Method                                #
          #-------------------------------------------------------------------------#
          accept = function(visitor)  {
            visitor$nlpStudio(self)
          },

          #-------------------------------------------------------------------------#
          #                             Test Methods                                #
          #-------------------------------------------------------------------------#
          exposeObject = function() {
            o <- list(
              className = private$..className,
              methodName = private$..methodName,
              name = private$..meta[["name"]],
              desc = private$..desc,
              path = private$..path,
              pipelines = private$..pipelines,
              logs = private$..logs,
              state = private$..state,
              created = private$..meta[["created"]],
              modified = private$..meta[["modified"]]
            )
            return(o)
          }

        )
      )
      super$initialize(...)
    }
  )
)#$new()
