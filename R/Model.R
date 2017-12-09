#==============================================================================#
#                                 Model                                        #
#==============================================================================#
#' Model
#'
#' \code{Model} Class in which corpora are created, processed, and transformed.
#'
#' There are two groups of methods. The core methods allow clients to
#' instantiate models and to obtain their basic information.  Aggregate methods
#' enable clients to add, retrieve and remove corpora from the model.
#'
#' \strong{Model Core Methods:}
#'  \describe{
#'   \item{\code{new(name, desc = NULL)}}{Creates an object of Model Class}
#'   \item{\code{desc}}{A getter/setter method allowing clients to retrieve and set the Model description variable.}
#'   \item{\code{getName()}}{Returns the name of the Model object.}
#'   \item{\code{getPath()}}{Returns the path of the Model object.}
#'   \item{\code{getLogs()}}{Returns the LogR object for the Model object.}
#'   \item{\code{logIt(level = 'Info', fieldName = NA)}}{Formats the log and calls the LogR class to log an event.}
#'  }
#'
#' \strong{Model Aggregate Methods:}
#'  \describe{
#'   \item{\code{getCorpora()}}{Retrieves the list of corpora for the model.}
#'   \item{\code{addCorpus(corpus)}}{Adds a model to the Model object.}
#'   \item{\code{removeCorpus(corpus)}}{Removes a model from the Model object. The model is archived in the NLPStudio archives.}
#' }
#'
#' \strong{Model Visitor Methods:}
#'  \describe{
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#' }
#'
#' @param name A character string containing the name of the Model object. This variable is used in the instantiation and remove methods.
#' @param desc A chararacter string containing the description of the Model
#' @param corpus An object of the Corpus class
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Model <- R6::R6Class(
  classname = "Model",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    ..corpora = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Model Core Methods                              #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL) {

      # Instantiate variables
      private$..className <- 'Model'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "model"), desc)
      private$..path <- file.path("./NLPStudio/models", name)
      private$..parent <- NLPStudio$new()$getInstance()
      private$..state <- paste("Model", name, "instantiated at", Sys.time())
      private$..logs <- LogR$new()
      private$..modified <- Sys.time()
      private$..created <- Sys.time()

      # Validate Model
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
    #                         Model Aggregate Methods                           #
    #-------------------------------------------------------------------------#
    getCorpora = function() { private$..corpora },

    addCorpus = function(corpus) {

      # Update current method
      private$..methodName <- 'addCorpus'

      # Validation
      v <- Validator$new()
      status <- v$addChild(self, corpus)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Get collection information
      corpusName <- corpus$getName()

      # Add collection to corpus's list of corpora
      private$..corpora[[corpusName]] <- corpus

      # Move corpora to corpus directory
      corpus$move(self)

      # Update modified time
      private$..modified <- Sys.time()

      # Save state and log Event
      private$..state <-
        paste("Corpus", corpusName, "added to Model", private$..name, "at", Sys.time())
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)

    },

    removeCorpus = function(corpus) {

      # Update current method
      private$..methodName <- 'removeCorpus'

      # Validation
      v <- Validator$new()
      status <- v$removeChild(self, corpus)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Obtain collection information
      corpusName <- corpus$getName()

      # Remove collection from corpus and update modified time
      private$..corpora[[corpusName]] <- NULL

      # Move corpora to main corpora directory
      corpus$move(NLPStudio$new()$getInstance())

      # Update modified time
      private$..modified <- Sys.time()

      # Save state and log Event
      private$..state <-
        paste("Corpus", corpusName, "removed from Model", private$..name, "at", Sys.time())
      self$logIt()

      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      name <- visitor$getName()
      visitor$model(self)
    },

    #-------------------------------------------------------------------------#
    #                           Test Methods                                  #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      #TODO: Remove after testing

      model = list(
        name = private$..name,
        desc = private$..desc,
        path = private$..path,
        parent = private$..parent,
        corpora = private$..corpora,
        logs <- private$..logs,
        state = private$..state,
        modified = private$..modified,
        created = private$..created
      )

      return(model)
    }
  )
)
