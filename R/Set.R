#==============================================================================#
#                                 Set                                          #
#==============================================================================#
#' Set
#'
#' \code{Set} Class that defines a set or collection of documents
#'
#' Class contains a collection of documents of various types, including text
#' documents, nGrams, and pos tagged documents.
#'
#' @section Set Core Methods:
#'  \describe{
#'   \item{\code{new(name, desc = NULL, lab = NULL)}}{Creates an object of Set Class}
#'   \item{\code{desc}}{A getter/setter method allowing clients to retrieve and set the Set description variable.}
#'   \item{\code{parent}}{A getter/setter method allowing clients to retrieve and set the Lab object to which the Set object belongs.}
#'   \item{\code{getName()}}{Returns the name of the Set object.}
#'   \item{\code{getPath()}}{Returns the path of the Set object.}
#'   \item{\code{getDocuments()}}{Returns the list of Document class objects.}
#'   \item{\code{addDocument(document)}}{Adds a Document object to the set.}
#'   \item{\code{removeDocument(document)}}{Removes a Document object from the set.}
#'   \item{\code{logIt(level = 'Info', fieldName = NA)}}{Formats the log and calls the LogR class to log an event.}
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#'  }
#'
#' @param name A character string containing the name of the Set object. This variable is used in the instantiation and remove methods.
#' @param desc A chararacter string containing the description of the Set.
#' @param document An object of one of the Document sub-classes.
#' @param visitor An object of one of the visitor classes.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Set <- R6::R6Class(
  classname = "Set",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..className = 'Set',
    ..methodName = character(),
    ..name = character(),
    ..desc = character(),
    ..parent = character(),
    ..path = character(),
    ..documents = list(),
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
                                 "set description changed at",
                                 Sys.time())
        self$logIt()
      }
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Set Instantiation                               #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL) {

      # Instantiate variables
      private$..className <- 'Set'
      private$..methodName <- 'initialize'
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "set"), desc)
      private$..parent <- NLPStudio$new()$getInstance()
      private$..path <- file.path(NLPStudio$new()$getInstance()$getPath(), 'sets', private$..name)
      private$..state <- "Set instantiated."
      private$..modified <- Sys.time()
      private$..created <- Sys.time()
      private$..logs <- LogR$new(file.path(NLPStudio$new()$getInstance()$getPath(), 'logs'))

      # Validate Set
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

    #-------------------------------------------------------------------------#
    #                         Basic Get Methods                               #
    #-------------------------------------------------------------------------#
    getName = function() private$..name,
    getClassName = function() private$..className,
    getPath = function() private$..path,

    #-------------------------------------------------------------------------#
    #                         Composite Methods                               #
    #-------------------------------------------------------------------------#
    getDocuments = function() private$..documents,

    addDocument = function(document) {

      # Update current method
      private$..methodName <- 'addDocument'

      # Validation
      v <- Validator$new()
      status <- v$addChild(self, document)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Get collection information
      documentName <- document$getName()

      # Add document to list of documents
      private$..documents[[documentName]] <- document

      #TODO: Move document to set directory

      # Document parent to document collection object
      document$parent <- self

      # Update modified time
      private$..modified <- Sys.time()

      # Save state and log Event
      private$..state <-
        paste("Document", documentName, "added to set,", private$..name, "at", Sys.time())
      self$logIt()

      # Assign its name in the global environment
      assign(private$..name, self, envir = .GlobalEnv)

      invisible(self)
    },

    removeDocument = function(document) {

      # Update current method
      private$..methodName <- 'removeDocument'

      # Validation
      v <- Validator$new()
      status <- v$removeChild(self, document)
      if (status[['code']] == FALSE) {
        private$..state <- status[['msg']]
        self$logIt(level = 'Error')
        stop()
      }

      # Obtain collection information
      documentName <- document$getName()

      # Remove collection from lab and update modified time
      private$..documents[[documentName]] <- NULL

      # Move document back to main document directory
      document$move(NLPStudio$new()$getInstance())

      # Update modified time
      private$..modified <- Sys.time()

      # Save state and log Event
      private$..state <-
        paste("Document", documentName, "removed from ", private$..name, "at", Sys.time())
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

        # Move Documents
        lapply(private$..documents, function(d) {
          d$move(self)
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
      visitor$set(self)
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

      set = list(
        className = private$..className,
        methodName = private$..methodName,
        name = private$..name,
        desc = private$..desc,
        path = private$..path,
        parent = private$..parent,
        documents = private$..documents,
        logs = private$..logs,
        state = private$..state,
        modified = private$..modified,
        created = private$..created
      )

      return(set)
    }
  )
)
