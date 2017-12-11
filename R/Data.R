#==============================================================================#
#                                 Data                                         #
#==============================================================================#
#' Data
#'
#' \code{Data} Class which contains all corpora and cross validation sets.
#'
#' Class containing the raw, refined, cross-validation and preprocessed corpora.
#' The cross-validation sets contain a single training, validation, and
#' test set.
#'
#' @section Data Methods:
#'  \describe{
#'   \item{\code{new(train, val, test)}}{Instantiates the Data object}
#'   \item{\code{getCorpora()}}{Retrieves the list of corpora from the data set.}
#'   \item{\code{addCorpus(corpus)}}{Adds a corpus to the data set.}
#'   \item{\code{removeCorpus(corpus))}}{Removes a corpus from the data set.}
#'   \item{\code{move(parent))}}{Moves the data set to the new parent object.}
#'  }
#'
#' @param corpus Corpus object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Data builder classes
#' @export
Data <- R6::R6Class(
  classname = "Data",
  lock_objects = FALSE,
  lock_class = FALSE,
  inherit = Entity,

  private = list(
    corpora = list()
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                             Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL) {

      # Instantiate variables
      private$..className <- 'Data'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "data set"), desc)
      private$..parent <- NLPStudios$new()$getInstance()
      private$..path <- file.path(NLPStudios$new()$getInstance()$getPath(), 'data')
      private$..state <- "Data object instantiated."
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..logs <- LogR$new()

      # Validate Corpus
      v <- Validator$new()
      status <- v$init(self)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Create directory
      dir.create(private$..path, showWarnings = FALSE,  recursive = TRUE)

      # Create log entry
      self$logIt()

      # Assign its name in the global environment
      assign(name, self, envir = .GlobalEnv)

      invisible(self)

    },

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

      # Get corpus information
      corpusName <- corpus$getName()

      # Add corpus to list of corpora
      private$..corpora[[corpusName]] <- corpus

      # Move corpus to Corpus
      corpus$move(self)

      # Update modified time
      private$..modified <- Sys.time()

      # Save state and log Event
      private$..state <-
        paste("Corpus", corpusName, "added to the data set at", Sys.time())
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

      # Remove collection from studio and update modified time
      private$..corpora[[corpusName]] <- NULL

      # Move corpus back to main corpus directory
      corpus$move(NLPStudios$new()$getInstance())

      # Update modified time
      private$..modified <- Sys.time()

      # Save state and log Event
      private$..state <-
        paste("Corpus", corpusName, "removed from the data set at", Sys.time())
      self$logIt()

      invisible(self)
    },

    move = function(parent) {

      private$..methodName <- 'move'

      v <- Validator$new()
      status <- v$setParent(self, parent)

      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      } else {
        private$..parent <- parent

        private$..path <- file.path(parent$getPath(),
                                    private$..name)
        private$..modified <- Sys.time()
        private$..state <- paste(private$..className, private$..name, 'moved to ',
                                 parent$getClassName(), parent$getName())
        self$logIt()

        # Assign its name in the global environment
        assign(private$..name, self, envir = .GlobalEnv)

        invisible(self)
      }
    },


    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$data(self)
    },

    #-------------------------------------------------------------------------#
    #                           Test Methods                                  #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      #TODO: Remove after testing

      data = list(
        className = private$..className,
        methodName = private$..methodName,
        name = private$..name,
        desc = private$..desc,
        path = private$..path,
        parent = private$..parent,
        corpora = private$..corpora,
        logs = private$..logs,
        state = private$..state,
        modified = private$..modified,
        created = private$..created
      )

      return(corpus)
    }

  )
)
