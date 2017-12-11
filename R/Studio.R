#==============================================================================#
#                                 Studio                                          #
#==============================================================================#
#' Studio
#'
#' \code{Studio} Class in which models are created, executed and evaluated.
#'
#' The environment in which NLP happens. There are two groups of methods. The
#' core methods allow clients to instantiate studios and to obtain their basic
#' information.  Aggregate methods enable clients to add, retrieve and remove
#' models from the studio.
#'
#' \strong{Studio Core Methods:}
#'  \describe{
#'   \item{\code{new(name, desc = NULL)}}{Creates an object of Studio Class}
#'   \item{\code{desc}}{A getter/setter method allowing clients to retrieve and set the Studio description variable.}
#'   \item{\code{getName()}}{Returns the name of the Studio object.}
#'   \item{\code{getPath()}}{Returns the path of the Studio object.}
#'   \item{\code{getLogs()}}{Returns the LogR object for the Studio object.}
#'   \item{\code{logIt(level = 'Info', fieldName = NA)}}{Formats the log and calls the LogR class to log an event.}
#'  }
#'
#' \strong{Studio Aggregate Methods:}
#'  \describe{
#'   \item{\code{getModels()}}{Retrieves the list of models for the studio.}
#'   \item{\code{addModel(model)}}{Adds a model to the Studio object.}
#'   \item{\code{removeModel(model)}}{Removes a model from the Studio object. The model is archived in the NLPStudios archives.}
#' }
#'
#'
#' \strong{Studio Visitor Methods:}
#'  \describe{
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#' }
#'
#' @param name A character string containing the name of the Studio object. This variable is used in the instantiation and remove methods.
#' @param desc A chararacter string containing the description of the Studio
#' @param model An object of the Model class
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Studio <- R6::R6Class(
  classname = "Studio",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..models = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Studio Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL) {

      # Instantiate variables
      private$..className <- 'Studio'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "studio"), desc)
      private$..path <- file.path("./NLPStudios/studios", name)
      private$..parent <- NLPStudios$new()$getInstance()
      private$..state <- paste("Studio", name, "instantiated at", Sys.time())
      private$..logs <- LogR$new()
      private$..modified <- Sys.time()
      private$..created <- Sys.time()

      # Validate Studio
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Create directory
      dir.create(private$..path, recursive = TRUE)

      # Create log entry
      self$logIt()

      # Assign its name in the global environment
      assign(name, self, envir = .GlobalEnv)

      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                         Studio Aggregate Methods                           #
    #-------------------------------------------------------------------------#
    getModels = function() { private$..models },

    addModel = function(model) {

      # Update current method
      private$..methodName <- 'addModel'

      # Validation
      v <- Validator$new()
      status <- v$addChild(self, model)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Get collection information
      modelName <- model$getName()

      # Add collection to studio's list of models
      private$..models[[modelName]] <- model

      # Move models to studio directory
      model$move(self)

      # Update modified time
      private$..modified <- Sys.time()

      # Save state and log Event
      private$..state <-
        paste("Model", modelName, "added to Studio", private$..name, "at", Sys.time())
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)

    },

    removeModel = function(model) {

      # Update current method
      private$..methodName <- 'removeModel'

      # Validation
      v <- Validator$new()
      status <- v$removeChild(self, model)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Obtain collection information
      modelName <- model$getName()

      # Remove collection from studio and update modified time
      private$..models[[modelName]] <- NULL

      # Move models to main models directory
      model$move(NLPStudios$new()$getInstance())

      # Update modified time
      private$..modified <- Sys.time()

      # Save state and log Event
      private$..state <-
        paste("Model", modelName, "removed from Studio", private$..name, "at", Sys.time())
      self$logIt()

      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      name <- visitor$getName()
      visitor$studio(self)
    },

    #-------------------------------------------------------------------------#
    #                           Test Methods                                  #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      #TODO: Remove after testing

      studio = list(
        name = private$..name,
        desc = private$..desc,
        path = private$..path,
        parent = private$..parent,
        models = private$..models,
        logs <- private$..logs,
        state = private$..state,
        modified = private$..modified,
        created = private$..created
      )

      return(studio)
    }
  )
)
