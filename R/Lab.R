#==============================================================================#
#                                 Lab                                          #
#==============================================================================#
#' Lab
#'
#' \code{Lab} Class in which models are created, executed and evaluated.
#'
#' The environment in which NLP happens. There are two groups of methods. The
#' core methods allow clients to instantiate labs and to obtain their basic
#' information.  Aggregate methods enable clients to add, retrieve and remove
#' models from the lab.
#'
#' \strong{Lab Core Methods:}
#'  \describe{
#'   \item{\code{new(name, desc = NULL)}}{Creates an object of Lab Class}
#'   \item{\code{desc}}{A getter/setter method allowing clients to retrieve and set the Lab description variable.}
#'   \item{\code{getName()}}{Returns the name of the Lab object.}
#'   \item{\code{getPath()}}{Returns the path of the Lab object.}
#'   \item{\code{getLogs()}}{Returns the LogR object for the Lab object.}
#'   \item{\code{logIt(level = 'Info', fieldName = NA)}}{Formats the log and calls the LogR class to log an event.}
#'  }
#'
#' \strong{Lab Aggregate Methods:}
#'  \describe{
#'   \item{\code{getModels()}}{Retrieves the list of models for the lab.}
#'   \item{\code{addModel(model)}}{Adds a model to the Lab object.}
#'   \item{\code{removeModel(model)}}{Removes a model from the Lab object. The model is archived in the NLPStudio archives.}
#' }
#'
#'
#' \strong{Lab Visitor Methods:}
#'  \describe{
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#' }
#'
#' @param name A character string containing the name of the Lab object. This variable is used in the instantiation and remove methods.
#' @param desc A chararacter string containing the description of the Lab
#' @param model An object of the Model class
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Lab <- R6::R6Class(
  classname = "Lab",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..models = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Lab Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL) {

      # Instantiate variables
      private$..className <- 'Lab'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "lab"), desc)
      private$..path <- file.path("./NLPStudio/labs", name)
      private$..parent <- NLPStudio$new()$getInstance()
      private$..state <- paste("Lab", name, "instantiated at", Sys.time())
      private$..logs <- LogR$new()
      private$..modified <- Sys.time()
      private$..created <- Sys.time()

      # Validate Lab
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
    #                         Lab Aggregate Methods                           #
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

      # Add collection to lab's list of models
      private$..models[[modelName]] <- model

      # Move models to lab directory
      model$move(self)

      # Update modified time
      private$..modified <- Sys.time()

      # Save state and log Event
      private$..state <-
        paste("Model", modelName, "added to Lab", private$..name, "at", Sys.time())
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

      # Remove collection from lab and update modified time
      private$..models[[modelName]] <- NULL

      # Move models to main models directory
      model$move(NLPStudio$new()$getInstance())

      # Update modified time
      private$..modified <- Sys.time()

      # Save state and log Event
      private$..state <-
        paste("Model", modelName, "removed from Lab", private$..name, "at", Sys.time())
      self$logIt()

      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      name <- visitor$getName()
      visitor$lab(self)
    },

    #-------------------------------------------------------------------------#
    #                           Test Methods                                  #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      #TODO: Remove after testing

      lab = list(
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

      return(lab)
    }
  )
)
