#==============================================================================#
#                                 Corpus                                       #
#==============================================================================#
#' Corpus
#'
#' \code{Corpus} Class that defines a corpus or collection of data sets
#'
#' Class defines the Corpus object as a collection of data sets or Set objects.
#'
#' @section Corpus Core Methods:
#'  \describe{
#'   \item{\code{new(name, desc = NULL, lab = NULL)}}{Creates an object of Corpus Class}
#'   \item{\code{desc}}{A getter/setter method allowing clients to retrieve and set the Corpus description variable.}
#'   \item{\code{parent}}{A getter/setter method allowing clients to retrieve and set the Lab object to which the Corpus object belongs.}
#'   \item{\code{getName()}}{Returns the name of the Corpus object.}
#'   \item{\code{getPath()}}{Returns the path of the Corpus object.}
#'   \item{\code{move(parent)}}{Moves the Corpus object to the new parent.}
#'   \item{\code{getSets()}}{Returns list of set objects.}
#'   \item{\code{addSet(set)}}{Adds a Set object to the Corpus object.}
#'   \item{\code{removeSet(set)}}{Removes a Set object from the Corpus object.}
#'   \item{\code{logIt(level = 'Info', fieldName = NA)}}{Formats the log and calls the LogR class to log an event.}
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#'  }
#'
#'
#' @param name A character string containing the name of the Corpus object. This variable is used in the instantiation and remove methods.
#' @param desc A chararacter string containing the description of the Corpus.
#' @param set An object of the Set class.
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Corpus <- R6::R6Class(
  classname = "Corpus",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..className = 'Corpus',
    ..methodName = character(),
    ..name = character(),
    ..desc = character(),
    ..parent = character(),
    ..path = character(),
    ..sets = character(),
    ..state = character(),
    ..logs = character(),
    ..modified = character(),
    ..created = character()
  ),

  active = list(

    desc = function(value) {
      if (missing(value)) {
        private$..desc
      } else {
        private$..desc <- value
        private$..modified <- Sys.time()
        private$..state <- paste(private$..name,
                                 "corpus description changed at",
                                 Sys.time())
        self$logIt()
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Corpus Instantiation                            #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL) {

      # Instantiate variables
      private$..className <- 'Corpus'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "corpus"), desc)
      private$..parent <- NLPStudio$new()$getInstance()
      private$..path <- file.path(NLPStudio$new()$getInstance()$getPath, 'corpora', private$..name)
      private$..state <- "Corpus instantiated."
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..logs <- LogR$new(NLPStudio$new()$getInstance()$getDirs()$logs)

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
    #                         Composite Methods                               #
    #-------------------------------------------------------------------------#
    getSets = function() private$..sets,

    addSet = function(set) {

      # Update current method
      private$..methodName <- 'addSet'

      # Validation
      v <- Validator$new()
      status <- v$addChild(self, set)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Get collection information
      setName <- set$getName()

      # Add set to list of sets
      private$..sets[[setName]] <- set

      # Move files to lab
      from <- set$getPath()
      to <- file.path(private$..path, setName)
      f <- FileManager$new()
      status <- f$moveFile(from, to)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Set parent to document collection object
      set$parent <- self

      # Update modified time
      private$..modified <- Sys.time()

      # Save state and log Event
      private$..state <-
        paste("Set", setName, "added to corpus", private$..name, "at", Sys.time())
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },

    removeSet = function(set) {

      # Update current method
      private$..methodName <- 'removeSet'

      # Validation
      v <- Validator$new()
      status <- v$removeChild(self, set)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Obtain collection information
      setName <- set$getName()

      # Remove collection from lab and update modified time
      private$..corpora[[setName]] <- NULL

      # Change parent of removed object to null
      set$parent <- NULL

      # Move files back to set directory
      from <- file.path(private$..path, setName)
      to <- file.path(NLPStudio$new()$getInstance()$getPath(), 'sets')
      f <- FileManager$new()
      status <- f$moveFile(from, to)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Update modified time
      private$..modified <- Sys.time()

      # Save state and log Event
      private$..state <-
        paste("Set", setName, "removed from ", private$..name, "at", Sys.time())
      self$logIt()

      invisible(self)

    },

    move = function(parent) {

      v <- Validator$new()
      status <- v$setParent(self, parent)

      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      } else {
        private$..parent <- parent

        # Move Sets
        lapply(private$..sets, function(s) {
          s$move(self)
        })

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
      visitor$corpus(self)
    },

    #-------------------------------------------------------------------------#
    #                            Log Method                                   #
    #-------------------------------------------------------------------------#
    logIt = function(level = 'Info', fieldName = NA) {

      private$..logs$entry$owner <- private$..name
      private$..logs$entry$className <- private$..className
      private$..logs$entry$methodName <- private$..methodName
      private$..logs$entry$level <- level
      private$..logs$entry$msg <- private$..state
      private$..logs$entry$fieldName <- fieldName
      private$..logs$created <- Sys.time()
      private$..logs$writeLog()
    },

    #-------------------------------------------------------------------------#
    #                           Test Methods                                  #
    #-------------------------------------------------------------------------#
    exposeObject = function() {

      #TODO: Remove after testing

      corpus = list(
        className = private$..className,
        methodName = private$..methodName,
        name = private$..name,
        desc = private$..desc,
        path = private$..path,
        parent = private$..parent,
        sets = private$..sets,
        logs = private$..logs,
        state = private$..state,
        modified = private$..modified,
        created = private$..created
      )

      return(corpus)
    }
  )
)
